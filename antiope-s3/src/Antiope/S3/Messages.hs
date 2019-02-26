{-# LANGUAGE DeriveGeneric #-}

module Antiope.S3.Messages
  ( EventName(..)
  , S3Message(..)
  , readEventName
  ) where

import Antiope.S3      (BucketName (..), ETag (..), ObjectKey (..))
import Data.Aeson      as Aeson
import Data.Int        (Int64)
import Data.Text       (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics    (Generic)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

data EventName = EventName
  { eventType :: !Text
  , action    :: !Text
  } deriving (Show, Eq, Ord, Generic)

data S3Message = S3Message
  { eventTime :: !(Maybe UTCTime)
  , eventName :: !EventName
  , bucket    :: !BucketName
  , key       :: !ObjectKey
  , size      :: !Int64
  , etag      :: !(Maybe ETag)
  } deriving (Show, Eq, Generic)

instance FromJSON S3Message where
  parseJSON = withObject "S3Message" $ \obj -> do
    eTime <- obj .:? "eventTime"
    eName <- readEventName <$> obj .: "eventName"
    s3    <- obj .: "s3"
    bkt   <- s3 .: "bucket"
    obj'  <- s3 .: "object"
    bName <- BucketName <$> (bkt .: "name")
    oKey  <- ObjectKey <$> (obj' .: "key")
    oSize <- obj' .: "size"
    oEtag  <- fmap (ETag . T.encodeUtf8) <$> (obj' .:? "eTag")
    pure $ S3Message eTime eName bName oKey oSize oEtag

readEventName :: T.Text -> EventName
readEventName evt =
  case T.splitOn ":" evt of
    [t, e]       -> EventName t e
    ["s3", t, e] -> EventName t e
    (t:e:_)      -> EventName t e
    _            -> EventName evt ""

