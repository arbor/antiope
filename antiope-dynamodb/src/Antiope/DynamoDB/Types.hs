{-# LANGUAGE DeriveGeneric #-}

module Antiope.DynamoDB.Types where

import Data.String           (IsString)
import Data.Text             (Text)
import GHC.Generics
import Network.AWS.Data.Text (FromText (..), ToText (..))

newtype TableName = TableName Text
  deriving (Eq, Show, IsString, ToText, FromText, Generic)
