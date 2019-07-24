{-# LANGUAGE DeriveGeneric #-}

module Antiope.DynamoDB.Types where

import Data.Aeson
import Data.String           (IsString)
import Data.Text             (Text)
import GHC.Generics
import Network.AWS.Data.Text (FromText (..), ToText (..))

newtype TableName = TableName Text
  deriving (Eq, Show, IsString, ToText, FromText, Generic)

instance ToJSON TableName where
  toJSON (TableName t) = toJSON t

instance FromJSON TableName where
  parseJSON v = TableName <$> parseJSON v
