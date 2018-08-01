module Antiope.SNS
  ( publishMessage
  , subscribeTopic
  , module Antiope.SNS.Types
  , module Network.AWS.SNS
  ) where

import Antiope.SNS.Types
import Control.Lens
import Data.Text         (Text)
import Network.AWS       (MonadAWS)
import Network.AWS.SNS

import qualified Network.AWS as AWS

publishMessage :: MonadAWS m
  => Topic
  -> Text
  -> m (Maybe MessageId)
publishMessage topicArn message = do
  resp <- AWS.send $ publish message & pTopicARN <>~ (topicArn ^. tTopicARN)
  return $ MessageId <$> resp ^. prsMessageId

subscribeTopic :: MonadAWS m
  => Topic
  -> Protocol
  -> Endpoint
  -> m (Maybe SubscriptionArn)
subscribeTopic topicArn (Protocol p) ep = case topicArn ^. tTopicARN of
  Just t -> do
    resp <- AWS.send $ subscribe t p & subEndpoint <>~ (ep ^. eEndpointARN)
    return $ SubscriptionArn <$> resp ^. srsSubscriptionARN
  Nothing -> return Nothing
