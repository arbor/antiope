module Antiope.SQS
( QueueUrl(..)
, SQSError(..)
, readQueue
, drainQueue
, ackMessage
, ackMessages
, messageInBody
, messageToS3Uri
, messageToS3Uri'
) where

import Antiope.S3          (S3Uri (..))
import Antiope.SQS.Types   (QueueUrl (QueueUrl), SQSError (DeleteMessageBatchError))
import Control.Lens
import Control.Monad       (join)
import Control.Monad.Loops (unfoldWhileM)
import Data.Aeson.Lens
import Data.Maybe          (catMaybes)
import Data.Text           (Text, pack)
import Network.AWS         (MonadAWS)
import Network.AWS.SQS

import qualified Antiope.Messages as Z
import qualified Network.AWS      as AWS

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

ackMessage :: MonadAWS m
  => QueueUrl
  -> Message
  -> m (Either SQSError ())
ackMessage q msg = ackMessages q [msg]

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

-- Extract the "Message" content in the body if have
messageInBody :: Text -> Maybe Text
messageInBody body = body ^? key "Message" . _String
{-# DEPRECATED messageInBody
    "The export of messageInBody is deprecated.  Use the one from Antiope.Messages instead" #-}

messageToS3Uri :: Message -> Maybe S3Uri
messageToS3Uri = Z.messageToS3Uri
{-# DEPRECATED messageToS3Uri
    "The export of messageToS3Uri is deprecated.  Use the one from Antiope.Messages instead" #-}

messageToS3Uri' :: Text -> Maybe S3Uri
messageToS3Uri' = Z.messageToS3Uri'
{-# DEPRECATED messageToS3Uri'
  "The export of messageToS3Uri' is deprecated.  Use the one from Antiope.Messages instead" #-}
