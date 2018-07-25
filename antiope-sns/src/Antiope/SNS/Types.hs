{-# LANGUAGE DeriveGeneric #-}

module Antiope.SNS.Types where

import Data.Text             (Text)
import GHC.Generics
import Network.AWS.Data.Text (FromText (..), ToText (..))

data SNSError = PublishMessageError
  deriving (Eq, Show, Generic)

newtype MessageId = MessageId Text deriving (Show, Eq, Ord, ToText, FromText)
newtype Protocol = Protocol Text deriving (Show, Eq, Ord, ToText, FromText)
newtype SubscriptionArn = SubscriptionArn Text deriving (Show, Eq, Ord, ToText, FromText)
