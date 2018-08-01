module Antiope.SNS.Types
  ( module X
  , MessageId(..)
  , Protocol(..)
  , SubscriptionArn(..)
  ) where

import Data.Text             (Text)
import Network.AWS.Data.Text (FromText (..), ToText (..))

import Network.AWS.SNS as X

newtype MessageId = MessageId Text deriving (Show, Eq, Ord, ToText, FromText)
newtype Protocol = Protocol Text deriving (Show, Eq, Ord, ToText, FromText)
newtype SubscriptionArn = SubscriptionArn Text deriving (Show, Eq, Ord, ToText, FromText)
