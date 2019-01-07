module Foreign.Class where

import Prelude

import Control.Monad.Except (except, mapExcept)
import Data.Array ((..), (!!), zipWith, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (sequence)
import Data.Tuple (Tuple (..))
import Foreign (F, Foreign, ForeignError(..), readArray, readBoolean, readChar, readInt, readNumber, readString, unsafeToForeign, fail)
import Foreign.Internal (readObject)
import Foreign.NullOrUndefined (readNullOrUndefined, undefined)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | The `Decode` class is used to generate decoding functions
-- | of the form `Foreign -> F a` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericDecode` and `genericDecodeJSON` functions
-- | to decode your foreign/JSON-encoded data.
class Decode a where
  decode :: Foreign -> F a

instance voidDecode :: Decode Void where
  decode _ = except (Left (pure (ForeignError "Decode: void")))

instance unitDecode :: Decode Unit where
  decode _ = pure unit

instance foreignDecode :: Decode Foreign where
  decode = pure

instance stringDecode :: Decode String where
  decode = readString

instance charDecode :: Decode Char where
  decode = readChar

instance booleanDecode :: Decode Boolean where
  decode = readBoolean

instance numberDecode :: Decode Number where
  decode = readNumber

instance intDecode :: Decode Int where
  decode = readInt

instance arrayDecode :: Decode a => Decode (Array a) where
  decode = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (decode value)

instance maybeDecode :: Decode a => Decode (Maybe a) where
  decode = readNullOrUndefined decode

instance objectDecode :: Decode v => Decode (Object v) where
  decode = sequence <<< Object.mapWithKey (\_ -> decode) <=< readObject

-- | The `Encode` class is used to generate encoding functions
-- | of the form `a -> Foreign` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericEncode` and `genericEncodeJSON` functions
-- | to encode your data as JSON.
class Encode a where
  encode :: a -> Foreign

instance voidEncode :: Encode Void where
  encode = absurd

instance unitEncode :: Encode Unit where
  encode _ = unsafeToForeign {}

instance foreignEncode :: Encode Foreign where
  encode = identity

instance stringEncode :: Encode String where
  encode = unsafeToForeign

instance charEncode :: Encode Char where
  encode = unsafeToForeign

instance booleanEncode :: Encode Boolean where
  encode = unsafeToForeign

instance numberEncode :: Encode Number where
  encode = unsafeToForeign

instance intEncode :: Encode Int where
  encode = unsafeToForeign

instance arrayEncode :: Encode a => Encode (Array a) where
  encode = unsafeToForeign <<< map encode

instance maybeEncode :: Encode a => Encode (Maybe a) where
  encode = maybe undefined encode

instance objectEncode :: Encode v => Encode (Object v) where
  encode = unsafeToForeign <<< Object.mapWithKey (\_ -> encode)

instance tupleEncode :: (Encode a, Encode b) => Encode (Tuple a b) where
  encode (Tuple a b) = encode [ encode a, encode b ]

instance tupleDecode :: (Decode a, Decode b) => Decode (Tuple a b) where
  decode fTuple = do
    arrF <- decode fTuple
    a <- maybe (fail (ForeignError "tupleDecode")) decode (arrF !! 0)
    b <- maybe (fail (ForeignError "tupleDecode")) decode (arrF !! 1)
    pure $ Tuple a b

instance mapEncode :: (Encode k, Encode v) => Encode (Map k v) where
  encode =
    let theArray = (identity :: forall a. Array a -> Array a)
    in encode <<< theArray <<< Map.toUnfoldable

instance mapDecode :: (Ord k, Decode k, Decode v) => Decode (Map k v) where
  decode = map (Map.fromFoldable <<< (identity :: forall a. Array a -> Array a)) <<< decode
