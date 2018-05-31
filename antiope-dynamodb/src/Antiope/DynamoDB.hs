{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.DynamoDB
( MonadAWS
, FromText(..), fromText
, ToText(..)
, TableName(..)
, dynamoPutItem
, dynamoQuery
, module Network.AWS.DynamoDB
) where

import           Control.Lens          ((&), (.~), (?~))
import           Data.HashMap.Strict   (HashMap)
import           Data.String           (IsString)
import           Data.Text             (Text)
import           Network.AWS           (MonadAWS, send)
import           Network.AWS.Data.Text (FromText (..), ToText (..), fromText,
                                        toText)
import           Network.AWS.DynamoDB

newtype TableName = TableName { unTableName :: Text } deriving (Eq, Show, IsString, ToText, FromText)

dynamoPutItem :: MonadAWS m => TableName -> HashMap Text AttributeValue -> m PutItemResponse
dynamoPutItem (TableName table) item =
  send $ putItem table & piItem .~ item

dynamoQuery :: MonadAWS m => TableName -> (Query -> Query) -> m QueryResponse
dynamoQuery (TableName table) f = send $ f $ query table
