module Antiope.SNS
( MonadAWS
, publishMsg
, subscribeMsg
, module Network.AWS.SNS
) where

import Control.Lens
import Data.Text       (Text)
import Network.AWS     (MonadAWS, send)
import Network.AWS.SNS

publishMsg :: MonadAWS m => Text -> Text -> m (Maybe Text)
publishMsg topicArn message = do
  resp <- send $ publish message & pTopicARN ?~ topicArn
  return $ resp ^. prsMessageId

subscribeMsg :: MonadAWS m => Text -> Text -> Text -> m (Maybe Text)
subscribeMsg topicArn protocol ep = do
  resp <- send $ subscribe topicArn protocol & subEndpoint ?~ ep
  return $ resp ^. srsSubscriptionARN
