{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.SQS
( MonadAWS
, FromText(..), fromText
, ToText(..)
, QueueUrl(..)
, readQueue
, drainQueue
, ackMessage
, ackMessages
, module Network.AWS.SQS
) where

import           Control.Lens          ((&), (?~), (^.))
import           Control.Monad         (forM_, join, void)
import           Control.Monad.Loops   (unfoldWhileM)
import           Network.AWS           (MonadAWS, send)
import           Network.AWS.SQS

import           Data.String           (IsString)
import           Data.Text             (Text)
import           Network.AWS.Data.Text (FromText (..), ToText (..), fromText,
                                        toText)

newtype QueueUrl = QueueUrl { unQueueUrl :: Text } deriving (Show, Eq, IsString, FromText, ToText)

-- | Reads the specified SQS queue once returning a bath of messages
readQueue :: (MonadAWS m) => QueueUrl -> m [Message]
readQueue (QueueUrl queueUrl) = do
  -- max wait time allowed by aws is 20sec, max number messages to recieve is 10
  resp <- send $ receiveMessage queueUrl & rmWaitTimeSeconds ?~ 10 & rmMaxNumberOfMessages ?~ 10
  return $ resp ^. rmrsMessages

-- | Reads the specified SQS queue until it is empty and returns a list of messages
drainQueue :: MonadAWS m => QueueUrl -> m [Message]
drainQueue queueUrl =
  join <$> unfoldWhileM (not . null) (readQueue queueUrl)

ackMessage :: MonadAWS m => QueueUrl -> Message -> m ()
ackMessage (QueueUrl queueUrl) msg =
  case msg ^. mReceiptHandle of
    Nothing -> pure ()
    Just rc -> void . send $ deleteMessage queueUrl rc

ackMessages :: MonadAWS m => QueueUrl -> [Message] -> m ()
ackMessages queueUrl messages =
  forM_ messages (ackMessage queueUrl)
