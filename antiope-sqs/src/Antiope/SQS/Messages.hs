{-# LANGUAGE DeriveGeneric #-}

module Antiope.SQS.Messages
  ( Many (..)
  , SqsMessage (..)
  ) where

import Data.Aeson   as Aeson
import Data.Text    (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson.Types   as Aeson
import qualified Data.Text.Encoding as T

newtype Many a = Many { records :: [a] } deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (Many a) where
  parseJSON =
    withObject "Many" $ \obj ->
      Many <$> obj .: "Records"

instance ToJSON a => ToJSON (Many a) where
  toJSON (Many a) =
    object ["Records" Aeson..= a]

data SqsMessage a = SqsMessage
  { senderId      :: !(Maybe Text)
  , messageId     :: !(Maybe Text)
  , md5OfBody     :: !(Maybe Text)
  , receiptHandle :: !(Maybe Text)
  , body          :: !(Maybe a)
  } deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (SqsMessage a) where
  parseJSON = withObject "SqsMessage" $ \obj -> SqsMessage
    <$> obj .:? "SenderId"
    <*> obj .:? "MessageId"
    <*> obj .:? "Md5OfBody"
    <*> obj .:? "ReceiptHandle"
    <*> decodeEscaped obj "Body"

decodeEscaped :: FromJSON b => Object -> Text -> Aeson.Parser b
decodeEscaped o t =
  (o .: t) >>= (either fail pure . eitherDecodeStrict . T.encodeUtf8)

