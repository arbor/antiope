module Antiope.SNS
( MonadAWS
, publishMessage
, subscribeTopic
, module Antiope.SNS.Types
, module Network.AWS.SNS
) where

import Antiope.SNS.Types
import Control.Lens
import Data.Text         (Text)
import Network.AWS       (MonadAWS, send)
import Network.AWS.SNS

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
