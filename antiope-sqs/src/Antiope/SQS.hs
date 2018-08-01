module Antiope.SQS
( FromText(..), fromText
, ToText(..)
, QueueUrl(..)
, SQSError(..)
, readQueue
, drainQueue
, ackMessage
, ackMessages
, messageInBody
, s3Location
, s3Location'
) where

import Antiope.S3              (S3Uri (..))
import Antiope.SQS.Types       (QueueUrl (QueueUrl), SQSError (DeleteMessageBatchError))
import Control.Lens
import Control.Monad           (join)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Loops     (unfoldWhileM)
import Data.Aeson.Lens
import Data.Maybe              (catMaybes)
import Data.Text               (Text, pack, unpack)
import Network.AWS             (HasEnv)
import Network.AWS.Data.Text   (FromText (..), ToText (..), fromText, toText)
import Network.AWS.S3          hiding (s3Location)
import Network.AWS.SQS

import qualified Network.AWS as AWS
import qualified Network.URI as URI

-- | Reads the specified SQS queue once returning a bath of messages
readQueue :: (HasEnv e, MonadUnliftIO m)
  => e
  -> QueueUrl
  -> m [Message]
readQueue e (QueueUrl queueUrl) = AWS.runResourceT . AWS.runAWS e $ do
  -- max wait time allowed by aws is 20sec, max number messages to recieve is 10
  resp <- AWS.send $ receiveMessage queueUrl & rmWaitTimeSeconds ?~ 10 & rmMaxNumberOfMessages ?~ 10
  return $ resp ^. rmrsMessages

-- | Reads the specified SQS queue until it is empty and returns a list of messages
drainQueue :: (HasEnv e, MonadUnliftIO m)
  => e
  -> QueueUrl
  -> m [Message]
drainQueue e queueUrl = join <$> unfoldWhileM (not . null) (readQueue e queueUrl)

ackMessage :: (HasEnv e, MonadUnliftIO m)
  => e
  -> QueueUrl
  -> Message
  -> m (Either SQSError ())
ackMessage e q msg = ackMessages e q [msg]

ackMessages :: (HasEnv e, MonadUnliftIO m)
  => e
  -> QueueUrl
  -> [Message]
  -> m (Either SQSError ())
ackMessages e (QueueUrl queueUrl) msgs = AWS.runResourceT . AWS.runAWS e $ do
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

s3Location :: Message -> Maybe S3Uri
s3Location msg = join $ s3Location' <$> msg ^. mBody

s3Location' :: Text -> Maybe S3Uri
s3Location' msg = do
  s3m <- messageInBody msg ^? _Just . key "Records" . nth 0 . key "s3"
  b   <- s3m ^? key "bucket" . key "name" . _String
  k   <- s3m ^? key "object" . key "key" . _String
  pure $ S3Uri (BucketName b) (ObjectKey $ uriDecode k)

uriDecode :: Text -> Text
uriDecode = pack . URI.unEscapeString . unpack
