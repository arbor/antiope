{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.DynamoDB
( MonadAWS
, FromText(..), fromText
, ToText(..), toText
, dynamoPutItem
, module Network.AWS.DynamoDB
) where

import           Control.Lens          ((&), (.~), (?~))
import           Data.HashMap.Strict   (HashMap)
import           Network.AWS           (MonadAWS, send)
import           Network.AWS.DynamoDB

import           Data.Text             (Text)
import           Network.AWS.Data.Text (FromText (..), ToText (..), fromText,
                                        toText)

dynamoPutItem :: MonadAWS m => Text -> HashMap Text AttributeValue -> m PutItemResponse
dynamoPutItem table item =
  send $ putItem table & piItem .~ item

