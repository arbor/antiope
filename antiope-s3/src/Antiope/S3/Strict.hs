{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.S3.Strict
  ( unsafeDownload
  , download
  , downloadFromS3Uri

  , unsafeDownloadMap
  , downloadMap
  , downloadMapFromS3Uri
  ) where

import Antiope.Core                 ()
import Antiope.S3.Types             (S3Uri)
import Control.Monad                ((<$!>))
import Control.Monad.Trans.AWS      hiding (send)
import Control.Monad.Trans.Resource
import Network.AWS                  (MonadAWS)

import qualified Antiope.S3.Lazy      as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.AWS.S3       as AWS

unsafeDownload :: (MonadAWS m, MonadUnliftIO m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m BS.ByteString
unsafeDownload bucketName objectKey = runResourceT $ LBS.toStrict <$!> LBS.unsafeDownload bucketName objectKey

download :: (MonadAWS m, MonadUnliftIO m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m (Maybe BS.ByteString)
download bucketName objectKey = runResourceT $  (LBS.toStrict <$!>) <$!> LBS.download bucketName objectKey

downloadFromS3Uri :: (MonadAWS m, MonadUnliftIO m)
  => S3Uri
  -> m (Maybe BS.ByteString)
downloadFromS3Uri s3Uri = runResourceT $ (LBS.toStrict <$!>) <$!> LBS.downloadFromS3Uri s3Uri

unsafeDownloadMap :: (MonadAWS m, MonadUnliftIO m)
  => (LBS.ByteString -> LBS.ByteString)
  -> AWS.BucketName
  -> AWS.ObjectKey
  -> m BS.ByteString
unsafeDownloadMap f bucketName objectKey = runResourceT $ LBS.toStrict <$!> (f <$> LBS.unsafeDownload bucketName objectKey)

downloadMap :: (MonadAWS m, MonadUnliftIO m)
  => (LBS.ByteString -> LBS.ByteString)
  -> AWS.BucketName
  -> AWS.ObjectKey
  -> m (Maybe BS.ByteString)
downloadMap f bucketName objectKey = runResourceT $ (LBS.toStrict <$!>) <$!> (fmap f <$> LBS.download bucketName objectKey)

downloadMapFromS3Uri :: (MonadAWS m, MonadUnliftIO m)
  => (LBS.ByteString -> LBS.ByteString)
  -> S3Uri
  -> m (Maybe BS.ByteString)
downloadMapFromS3Uri f s3Uri = runResourceT $ (LBS.toStrict <$!>) <$!> (fmap f <$> LBS.downloadFromS3Uri s3Uri)
