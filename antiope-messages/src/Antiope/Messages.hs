module Antiope.Messages
  ( QueueUrl(..)
  , SQSError(..)
  , messageInBody
  , messageToS3Uri
  , messageToS3Uri'
  ) where

import Antiope.Messages.Types (QueueUrl (QueueUrl), SQSError (DeleteMessageBatchError))
import Antiope.S3             (S3Uri (..))
import Control.Lens
import Control.Monad          (join)
import Data.Aeson.Lens
import Data.Text              (Text, pack, unpack)
import Network.AWS.S3         (BucketName (BucketName), ObjectKey (ObjectKey))
import Network.AWS.SQS

import qualified Network.URI as URI

-- Extract the "Message" content in the body if have
messageInBody :: Text -> Maybe Text
messageInBody body = body ^? key "Message" . _String

messageToS3Uri :: Message -> Maybe S3Uri
messageToS3Uri msg = join $ messageToS3Uri' <$> msg ^. mBody

messageToS3Uri' :: Text -> Maybe S3Uri
messageToS3Uri' msg = do
  s3m <- messageInBody msg ^? _Just . key "Records" . nth 0 . key "s3"
  b   <- s3m ^? key "bucket" . key "name" . _String
  k   <- s3m ^? key "object" . key "key" . _String
  pure $ S3Uri (BucketName b) (ObjectKey $ uriDecode k)

uriDecode :: Text -> Text
uriDecode = pack . URI.unEscapeString . unpack
