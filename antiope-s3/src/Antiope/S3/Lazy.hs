{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.S3.Lazy
  ( unsafeDownload
  , download
  , downloadFromS3Uri
  ) where

import Antiope.S3.GetObject
import Antiope.S3.Types             (S3Uri (S3Uri))
import Control.Lens
import Control.Monad.Catch          (catch)
import Control.Monad.Trans.Resource
import Network.AWS                  (Error (..), MonadAWS, ServiceError (..))
import Network.HTTP.Types.Status    (Status (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Network.AWS.S3       as AWS

unsafeDownload :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m LBS.ByteString
unsafeDownload bucketName objectKey = do
  resp <- getObject bucketName objectKey
  lazyByteString (resp ^. AWS.gorsBody)

download :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m (Maybe LBS.ByteString)
download bucketName objectKey = do
  ebs <- (Right <$> unsafeDownload bucketName objectKey) `catch` \(err :: Error) -> case err of
    (ServiceError (ServiceError' _ (Status 404 _) _ _ _ _)) -> return (Left LBS.empty)
    _                                                       -> throwM err
  case ebs of
    Right bs -> return (Just bs)
    Left _   -> return Nothing

downloadFromS3Uri :: (MonadAWS m, MonadResource m)
  => S3Uri
  -> m (Maybe LBS.ByteString)
downloadFromS3Uri (S3Uri b k) = download b k
