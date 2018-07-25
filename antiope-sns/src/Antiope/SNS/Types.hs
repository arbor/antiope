module Antiope.SNS.Types where

import Data.Text             (Text)
import Network.AWS.Data.Text (FromText (..), ToText (..))

newtype MessageId = MessageId Text deriving (Show, Eq, Ord, ToText, FromText)
newtype Protocol = Protocol Text deriving (Show, Eq, Ord, ToText, FromText)
newtype SubscriptionArn = SubscriptionArn Text deriving (Show, Eq, Ord, ToText, FromText)
