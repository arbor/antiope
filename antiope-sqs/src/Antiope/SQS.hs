{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.SQS
( MonadAWS
, FromText(..), fromText
, ToText(..)
, QueueUrl(..)
, SQSError(..)
, readQueue
, drainQueue
, ackMessage
, ackMessages
, module Network.AWS.SQS
) where

import Control.Lens         (each, (&), (.~), (?~), (^.), (^..))
import Control.Monad        (forM_, join, void, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Loops  (unfoldWhileM)
import Data.Maybe           (catMaybes)
import Network.AWS          (MonadAWS, send)
import Network.AWS.SQS

import Data.String           (IsString)
import Data.Text             (Text, pack)
import Network.AWS.Data.Text (FromText (..), ToText (..), fromText, toText)

data SQSError = DeleteMessageBatchError

newtype QueueUrl = QueueUrl { unQueueUrl :: Text } deriving (Show, Eq, IsString, FromText, ToText)

-- | Reads the specified SQS queue once returning a bath of messages
readQueue :: MonadAWS m => QueueUrl -> m [Message]
readQueue (QueueUrl queueUrl) = do
  -- max wait time allowed by aws is 20sec, max number messages to recieve is 10
  resp <- send $ receiveMessage queueUrl & rmWaitTimeSeconds ?~ 10 & rmMaxNumberOfMessages ?~ 10
  return $ resp ^. rmrsMessages

-- | Reads the specified SQS queue until it is empty and returns a list of messages
drainQueue :: MonadAWS m => QueueUrl -> m [Message]
drainQueue queueUrl =
  join <$> unfoldWhileM (not . null) (readQueue queueUrl)

ackMessage :: MonadAWS m => QueueUrl -> Message -> ExceptT SQSError m ()
ackMessage q msg = ackMessages q [msg]

ackMessages :: MonadAWS m => QueueUrl -> [Message] -> ExceptT SQSError m ()
ackMessages (QueueUrl queueUrl) msgs = do
  let receipts = catMaybes $ msgs ^.. each . mReceiptHandle
  -- each dmbr needs an ID. just use the list index.
  let dmbres = (\(r, i) -> deleteMessageBatchRequestEntry (pack (show i)) r) <$> zip receipts ([0..] :: [Int])
  resp <- send $ deleteMessageBatch queueUrl & dmbEntries .~ dmbres
  -- only acceptable if no errors.
  when (resp ^. dmbrsResponseStatus == 200) $
      case resp ^. dmbrsFailed of
        [] -> return ()
        _  -> throwError DeleteMessageBatchError
