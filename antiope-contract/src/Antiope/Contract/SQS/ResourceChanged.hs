{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Antiope.Contract.SQS.ResourceChanged where

import Data.Aeson
import Data.Avro.Deriving
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

-- automagically creates data types and ToAvro/FromAvro instances
-- for a given schema.
flip deriveAvroWithOptions "contract/resource_changed.avsc" $ defaultDeriveOptions
  { fieldNameBuilder = mkAsIsFieldName
  }

instance ToJSON ResourceChanged where
  toJSON r = object
    [ "eventTime" .= eventTime r
    , "uri"       .= uri r
    ]

instance FromJSON ResourceChanged where
  parseJSON = withObject "ResourceChanged" $ \o ->
    ResourceChanged <$> o .: "eventTime"
                    <*> o .: "uri"

toResourceChanged :: T.Text -> Maybe ResourceChanged
toResourceChanged = J.decode . LBS.fromStrict . encodeUtf8

rcmUri :: T.Text -> Maybe T.Text
rcmUri body = toResourceChanged body >>= \m -> pure $ uri m
