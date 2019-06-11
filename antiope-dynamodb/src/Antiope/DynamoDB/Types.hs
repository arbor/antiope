{-# LANGUAGE DeriveGeneric #-}

module Antiope.DynamoDB.Types where

import Control.Concurrent.Chan.Unagi.Bounded (OutChan)
import Data.HashMap.Strict                   (HashMap)
import Data.String                           (IsString)
import Data.Text                             (Text)
import GHC.Generics                          (Generic)
import GHC.Natural                           (Natural)
import Network.AWS.Data.Text                 (FromText (..), ToText (..))
import Network.AWS.DynamoDBStreams           (Record)

newtype StreamIterator = StreamIterator (OutChan Record) deriving Generic

newtype TableName = TableName Text
  deriving (Eq, Show, Ord, IsString, ToText, FromText, Generic)

newtype StreamArn = StreamArn Text
  deriving (Eq, Show, Ord, IsString, ToText, FromText, Generic)

newtype ShardId = ShardId Text
  deriving (Eq, Show, Ord, IsString, ToText, FromText, Generic)

newtype ShardIterator = ShardIterator Text
  deriving (Eq, Show, IsString, ToText, FromText, Generic)

newtype Limit = Limit Natural
  deriving (Eq, Show, Ord, Generic)
