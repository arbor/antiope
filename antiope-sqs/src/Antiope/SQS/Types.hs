{-# LANGUAGE DeriveGeneric #-}

module Antiope.SQS.Types where

import Data.String           (IsString)
import Data.Text             (Text)
import GHC.Generics
import Network.AWS.Data.Text (FromText (..), ToText (..))

data SQSError = DeleteMessageBatchError
  deriving (Eq, Show, Generic)

newtype QueueUrl = QueueUrl
  { text :: Text
  } deriving (Show, Eq, IsString, FromText, ToText, Generic)
