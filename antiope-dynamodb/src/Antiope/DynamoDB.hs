{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.DynamoDB
( TableName(..)
, dynamoPutItem
, dynamoQuery
, module Network.AWS.DynamoDB
) where

import Antiope.Core            (ToText (..))
import Antiope.DynamoDB.Types  (TableName (TableName))
import Control.Lens
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.HashMap.Strict     (HashMap)
import Data.Text               (Text)
import Network.AWS             (HasEnv)
import Network.AWS.DynamoDB

import qualified Network.AWS as AWS

dynamoPutItem :: (MonadUnliftIO m, HasEnv e)
  => e
  -> TableName
  -> HashMap Text AttributeValue
  -> m PutItemResponse
dynamoPutItem e table item = AWS.runResourceT $ AWS.runAWS e $ AWS.send $ putItem (table & toText) & piItem .~ item

dynamoQuery :: (MonadUnliftIO m, HasEnv e)
  => e
  -> TableName
  -> (Query
  -> Query)
  -> m QueryResponse
dynamoQuery e table f = AWS.runResourceT $ AWS.runAWS e $ AWS.send $ f $ query (table & toText)
