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
, fromWith2, fromWith3
) where

import Data.Aeson   (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, withObject, (.:), (.=))
import Data.Text    (Text)
import GHC.TypeLits

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text

-- | Extracts value from 'With' and 'WithEncoded' wrappers
class FromWith f where
  fromWith :: f a -> a -- ^ Extracts value from 'With' and 'WithEncoded'

instance FromWith (With x) where
  fromWith (With a) = a

instance FromWith (WithEncoded x) where
  fromWith (WithEncoded a) = a

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

-- | Represents a JSON value of type 'a' that is encoded as a string in a field 'fld'
data WithEncoded (fld :: Symbol) a where
  WithEncoded :: forall fld a. KnownSymbol fld => a -> WithEncoded fld a

-- | Represents a JSON value of type 'a' in a field 'fld'
data With (fld :: Symbol) a where
  With :: forall fld a. KnownSymbol fld => a -> With fld a

instance Show a => Show (WithEncoded fld a) where
  show (WithEncoded a) = show a

instance Show a => Show (With fld a) where
  show (With a) = show a

instance Eq a => Eq (WithEncoded fld a) where
  (WithEncoded a) == (WithEncoded b) = a == b

instance Eq a => Eq (With fld a) where
  (With a) == (With b) = a == b

instance Ord a => Ord (WithEncoded fld a) where
  compare (WithEncoded a) (WithEncoded b) = compare a b

instance Ord a => Ord (With fld a) where
  compare (With a) (With b) = compare a b

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
