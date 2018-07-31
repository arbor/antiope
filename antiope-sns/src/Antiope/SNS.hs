module Antiope.SNS
  ( publishMessage
  , subscribeTopic
  , module Antiope.SNS.Types
  , module Network.AWS.SNS
  ) where

import Antiope.SNS.Types
import Control.Lens
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text               (Text)
import Network.AWS             (HasEnv)
import Network.AWS.SNS

import qualified Network.AWS as AWS

publishMessage :: (MonadUnliftIO m, HasEnv e)
  => e
  -> Topic
  -> Text
  -> m (Maybe MessageId)
publishMessage e topicArn message = do
  resp <- AWS.runResourceT $ AWS.runAWS e $ AWS.send $ publish message & pTopicARN <>~ (topicArn ^. tTopicARN)
  return $ MessageId <$> resp ^. prsMessageId

subscribeTopic :: (MonadUnliftIO m, HasEnv e)
  => e
  -> Topic
  -> Protocol
  -> Endpoint
  -> m (Maybe SubscriptionArn)
subscribeTopic e topicArn (Protocol p) ep = case topicArn ^. tTopicARN of
  Just t -> do
    resp <- AWS.runResourceT $ AWS.runAWS e $ AWS.send $ subscribe t p & subEndpoint <>~ (ep ^. eEndpointARN)
    return $ SubscriptionArn <$> resp ^. srsSubscriptionARN
  Nothing -> return Nothing
