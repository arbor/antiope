module Antiope.SQS
  ( QueueUrl(..)
  , SQSError(..)
  , readQueue
  , drainQueue
  , ackMessage
  , ackMessages
  , mBody
  , queueSource
  ) where

import Antiope.Messages         (QueueUrl (QueueUrl), SQSError (DeleteMessageBatchError))
import Control.Lens
import Control.Monad            (join)
import Control.Monad.Loops      (unfoldWhileM)
import Control.Monad.Trans      (lift)
import Data.Conduit
import Data.Conduit.Combinators (yieldMany)
import Data.Maybe               (catMaybes)
import Data.Text                (pack)
import Network.AWS              (MonadAWS)
import Network.AWS.SQS

import qualified Network.AWS as AWS

-- | Reads the specified SQS queue once returning a bath of messages
readQueue :: MonadAWS m
  => QueueUrl
  -> m [Message]
readQueue (QueueUrl queueUrl) = do
  -- max wait time allowed by aws is 20sec, max number messages to recieve is 10
  resp <- AWS.send $ receiveMessage queueUrl & rmWaitTimeSeconds ?~ 10 & rmMaxNumberOfMessages ?~ 10
  return $ resp ^. rmrsMessages

-- | Reads the specified SQS queue until it is empty and returns a list of messages
drainQueue :: MonadAWS m
  => QueueUrl
  -> m [Message]
drainQueue queueUrl = join <$> unfoldWhileM (not . null) (readQueue queueUrl)

-- | Acknowledges a single SQS message
ackMessage :: MonadAWS m
  => QueueUrl
  -> Message
  -> m (Either SQSError ())
ackMessage q msg = ackMessages q [msg]

-- | Acknowledges a group of SQS messages
ackMessages :: MonadAWS m
  => QueueUrl
  -> [Message]
  -> m (Either SQSError ())
ackMessages (QueueUrl queueUrl) msgs = do
  let receipts = catMaybes $ msgs ^.. each . mReceiptHandle
  -- each dmbr needs an ID. just use the list index.
  let dmbres = (\(r, i) -> deleteMessageBatchRequestEntry (pack (show i)) r) <$> zip receipts ([0..] :: [Int])
  resp <- AWS.send $ deleteMessageBatch queueUrl & dmbEntries .~ dmbres
  -- only acceptable if no errors.
  if resp ^. dmbrsResponseStatus == 200
    then case resp ^. dmbrsFailed of
        [] -> return $ Right ()
        _  -> return $ Left DeleteMessageBatchError
    else return $ Left DeleteMessageBatchError

-- | Reads from an SQS indefinitely, producing messages into a conduit
queueSource :: MonadAWS m => QueueUrl -> ConduitT () Message m ()
queueSource (QueueUrl queueUrl) = do
  m <- lift $ AWS.send $ receiveMessage queueUrl & rmWaitTimeSeconds ?~ 10 & rmMaxNumberOfMessages ?~ 10
  yieldMany (m ^. rmrsMessages)
  queueSource (QueueUrl queueUrl)


