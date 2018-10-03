{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Antiope.Contract.SQS.FileChangeMessage where

import Antiope.S3
import Data.Aeson
import Data.Avro.Deriving
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Network.URI          as URI

-- automagically creates data types and ToAvro/FromAvro instances
-- for a given schema.
flip deriveAvroWithOptions "contract/file_change_message.avsc" $ defaultDeriveOptions
  { fieldNameBuilder = mkAsIsFieldName
  }

instance ToJSON FileChangeMessage where
  toJSON r = object
    [ "eventName"   .= eventName r
    , "eventTime"   .= eventTime r
    , "bucketName"  .= bucketName r
    , "objectKey"   .= objectKey r
    , "objectSize"  .= objectSize r
    , "objectTag"   .= objectTag r
    ]

instance FromJSON FileChangeMessage where
  parseJSON = withObject "FileChangeMessage" $ \o ->
    FileChangeMessage <$> o .: "eventName"
                      <*> o .: "eventTime"
                      <*> o .: "bucketName"
                      <*> (T.pack . URI.unEscapeString . T.unpack <$> o .: "objectKey")
                      <*> o .: "objectSize"
                      <*> o .: "objectTag"

toFileChangeMessage :: T.Text -> Maybe FileChangeMessage
toFileChangeMessage = J.decode . LBS.fromStrict . encodeUtf8

fcmS3Uri :: T.Text -> Maybe S3Uri
fcmS3Uri body = toFileChangeMessage body >>= \m ->
  let b = BucketName (bucketName m)
      k = ObjectKey (objectKey m)
  in  pure $ S3Uri b k
