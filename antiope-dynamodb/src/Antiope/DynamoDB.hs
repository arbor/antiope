{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Antiope.DynamoDB
( MonadAWS
, FromText(..), fromText
, ToText(..)
, TableName(..)
, dynamoPutItem
, dynamoQuery
, module Network.AWS.DynamoDB
) where

import Antiope.DynamoDB.Types    (TableName (TableName))
import Control.Lens
import Data.Generics.Product.Any
import Data.HashMap.Strict       (HashMap)
import Data.Text                 (Text)
import Network.AWS               (MonadAWS, send)
import Network.AWS.Data.Text     (FromText (..), ToText (..), fromText, toText)
import Network.AWS.DynamoDB

dynamoPutItem :: MonadAWS m => TableName -> HashMap Text AttributeValue -> m PutItemResponse
dynamoPutItem table item = send $ putItem (table ^. the @"text") & piItem .~ item

dynamoQuery :: MonadAWS m => TableName -> (Query -> Query) -> m QueryResponse
dynamoQuery table f = send $ f $ query (table ^. the @"text")
