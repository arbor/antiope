module Antiope.SNS
( MonadAWS
, MessageId (..)
, Protocol (..)
, SubscriptionArn (..)
, publishMessage
, subscribeTopic
, module Network.AWS.SNS
) where

import Control.Lens
import Data.Text             (Text)
import Network.AWS           (MonadAWS, send)
import Network.AWS.Data.Text (FromText (..), ToText (..))
import Network.AWS.SNS

newtype MessageId = MessageId Text deriving (Show, Eq, Ord, ToText, FromText)
newtype Protocol = Protocol Text deriving (Show, Eq, Ord, ToText, FromText)
newtype SubscriptionArn = SubscriptionArn Text deriving (Show, Eq, Ord, ToText, FromText)

publishMessage :: MonadAWS m => Topic -> Text -> m (Maybe MessageId)
publishMessage topicArn message = do
  resp <- send $ publish message & pTopicARN <>~ (topicArn ^. tTopicARN)
  return $ MessageId <$> resp ^. prsMessageId

subscribeTopic :: MonadAWS m => Topic -> Protocol -> Endpoint -> m (Maybe SubscriptionArn)
subscribeTopic topicArn (Protocol p) ep = case topicArn ^. tTopicARN of
  Just t -> do
    resp <- send $ subscribe t p & subEndpoint <>~ (ep ^. eEndpointARN)
    return $ SubscriptionArn <$> resp ^. srsSubscriptionARN
  Nothing -> return Nothing
