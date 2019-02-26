{-# LANGUAGE DeriveGeneric #-}

module Antiope.SNS.Messages
  ( SnsMessage (..)
  ) where

import Data.Aeson      as Aeson
import Data.Text       (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics    (Generic)

import qualified Data.Aeson.Types   as Aeson
import qualified Data.Text.Encoding as T

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

decodeEscaped :: FromJSON b => Object -> Text -> Aeson.Parser b
decodeEscaped o t =
  (o .: t) >>= (either fail pure . eitherDecodeStrict . T.encodeUtf8)
