{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Antiope.Messages.Types
  ( WithEncoded(..)
  , With(..)
  , FromWith(..)
  , fromWith2, fromWith3, fromWith4, fromWith5, fromWith6
  ) where

import Data.Aeson   (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, withObject, (.:), (.=))
import Data.Coerce  (coerce)
import Data.Proxy
import Data.Text    (Text)
import GHC.TypeLits

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text

-- | Extracts value from 'With' and 'WithEncoded' wrappers
--
-- Will probably be deprecated soon, try using 'Data.Coerce.coerce' instead.
class FromWith f where
  fromWith :: f a -> a -- ^ Extracts value from 'With' and 'WithEncoded'

instance FromWith (With x) where
  fromWith = coerce

instance FromWith (WithEncoded x) where
  fromWith = coerce

-- | Extracts a value from any combination of two 'With' and/or 'WithEncoded'
--
-- @
-- fromWith2 @(With "one" (WithEncoded "two" True)) == True
-- @
fromWith2 :: (FromWith f, FromWith g) => f (g a) -> a
fromWith2 = fromWith . fromWith
{-# INLINE fromWith2 #-}

-- | Extracts a value from any combination of two 'With' and/or 'WithEncoded'
--
-- @
-- fromWith3 @(With "one" (WithEncoded "two" (With "three" True))) == True
-- @
fromWith3 :: (FromWith f, FromWith g, FromWith h) => f (g (h a)) -> a
fromWith3 = fromWith . fromWith . fromWith
{-# INLINE fromWith3 #-}

fromWith4 :: (FromWith f, FromWith g, FromWith h, FromWith k) => f (g (h (k a))) -> a
fromWith4 = fromWith . fromWith . fromWith . fromWith
{-# INLINE fromWith4 #-}

fromWith5 :: (FromWith f, FromWith g, FromWith h, FromWith k, FromWith p) => f (g (h (k (p a)))) -> a
fromWith5 = fromWith . fromWith . fromWith . fromWith . fromWith
{-# INLINE fromWith5 #-}

fromWith6 :: (FromWith f, FromWith g, FromWith h, FromWith k, FromWith p, FromWith q) => f (g (h (k (p (q a))))) -> a
fromWith6 = fromWith . fromWith . fromWith . fromWith . fromWith . fromWith
{-# INLINE fromWith6 #-}

-- | Represents a JSON value of type 'a' that is encoded as a string in a field 'fld'
newtype WithEncoded (fld :: Symbol) a = WithEncoded a deriving (Show, Eq, Ord)

-- | Represents a JSON value of type 'a' in a field 'fld'
newtype With (fld :: Symbol) a = With a deriving (Show, Eq, Ord)

instance (KnownSymbol fld, FromJSON a) => FromJSON (WithEncoded fld a) where
  parseJSON =
    let name = symbolVal @fld Proxy
    in withObject name $ \obj ->
        WithEncoded <$> decodeEscaped obj (Text.pack name)

instance (KnownSymbol fld, ToJSON a) => ToJSON (WithEncoded fld a) where
  toJSON (WithEncoded a) =
    let name = Text.pack (symbolVal @fld Proxy)
    in Aeson.object [ name .= (Text.decodeUtf8 . LBS.toStrict . encode) a ]

instance (KnownSymbol fld, FromJSON a) => FromJSON (With fld a) where
  parseJSON =
    let name = symbolVal @fld Proxy
    in withObject name $ \obj ->
        With <$> obj .: Text.pack name

instance (KnownSymbol fld, ToJSON a) => ToJSON (With fld a) where
  toJSON (With a) =
    let name = Text.pack (symbolVal @fld Proxy)
    in Aeson.object [ name .= a ]

decodeEscaped :: FromJSON b => Aeson.Object -> Text -> Aeson.Parser b
decodeEscaped o t =
  (o .: t) >>= (either fail pure . eitherDecodeStrict . Text.encodeUtf8)
