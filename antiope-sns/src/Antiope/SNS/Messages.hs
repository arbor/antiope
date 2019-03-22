{-# LANGUAGE DeriveGeneric #-}

module Antiope.SNS.Messages
  ( SnsMessage (..)
  ) where

import Data.Aeson      as Aeson
import Data.Text       (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics    (Generic)

import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as Text

data SnsMessage a = SnsMessage
  { type'     :: !(Maybe Text)
  , messageId :: !(Maybe Text)
  , topicArn  :: !(Maybe Text)
  , subject   :: !(Maybe Text)
  , timestamp :: !(Maybe UTCTime)
  , message   :: !a
  } deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (SnsMessage a) where
  parseJSON = withObject "SnsMessage" $ \obj -> SnsMessage
    <$> obj .:? "Type"
    <*> obj .:? "MessageId"
    <*> obj .:? "TopicArn"
    <*> obj .:? "Subject"
    <*> obj .:? "Timestamp"
    <*> decodeEscaped obj "Message"

instance ToJSON a => ToJSON (SnsMessage a) where
  toJSON msg = object
    [ "Type"      .= type' msg
    , "MessageId" .= messageId msg
    , "TopicArn"  .= topicArn msg
    , "Subject"   .= subject msg
    , "Timestamp" .= timestamp msg
    , "Message"   .= (Text.decodeUtf8 . LBS.toStrict . encode . message) msg
    ]

decodeEscaped :: FromJSON b => Object -> Text -> Aeson.Parser b
decodeEscaped o t =
  (o .: t) >>= (either fail pure . eitherDecodeStrict . Text.encodeUtf8)
