{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.S3.Lazy
  ( unsafeDownload
  , unsafeDownloadRequest
  , download
  , downloadRequest
  , downloadFromS3Uri
  ) where

import Antiope.Core.Error
import Antiope.S3.GetObject
import Antiope.S3.Types             (S3Uri (S3Uri))
import Control.Lens
import Control.Monad.Trans.Resource
import Network.AWS                  (MonadAWS)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.AWS          as AWS
import qualified Network.AWS.S3       as AWS

unsafeDownloadRequest :: (MonadAWS m, MonadResource m)
  => AWS.GetObject
  -> m LBS.ByteString
unsafeDownloadRequest req = do
  resp <- AWS.send req
  lazyByteString (resp ^. AWS.gorsBody)

unsafeDownload :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m LBS.ByteString
unsafeDownload bucketName objectKey = unsafeDownloadRequest (AWS.getObject bucketName objectKey)

downloadRequest :: (MonadAWS m, MonadResource m)
  => AWS.GetObject
  -> m (Maybe LBS.ByteString)
downloadRequest req = handle404ToNone (unsafeDownloadRequest req)

download :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m (Maybe LBS.ByteString)
download bucketName objectKey = downloadRequest (AWS.getObject bucketName objectKey)

downloadFromS3Uri :: (MonadAWS m, MonadResource m)
  => S3Uri
  -> m (Maybe LBS.ByteString)
downloadFromS3Uri (S3Uri b k) = download b k
