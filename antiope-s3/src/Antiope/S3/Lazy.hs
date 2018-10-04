{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.S3.Lazy
  ( lazyByteString
  , getObject
  , getObjectForS3Uri
  , unsafeDownloadLbs
  , downloadLbs
  , downloadLbsFromS3Uri
  ) where

import Antiope.S3.Types             (S3Uri (S3Uri))
import Control.Lens
import Control.Monad.Catch          (catch)
import Control.Monad.Trans.AWS      hiding (send)
import Control.Monad.Trans.Resource
import Data.Conduit.Lazy            (lazyConsume)
import Network.AWS                  (Error (..), MonadAWS, ServiceError (..))
import Network.AWS.Data.Body        (_streamBody)
import Network.HTTP.Types.Status    (Status (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Network.AWS          as AWS
import qualified Network.AWS.S3       as AWS

-- | Access the response body as a lazy bytestring
lazyByteString :: MonadResource m => RsBody -> m LBS.ByteString
lazyByteString rsBody = liftResourceT $ LBS.fromChunks <$> lazyConsume (_streamBody rsBody)

getObject :: MonadAWS m
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m (AWS.Rs AWS.GetObject)
getObject bucketName objectKey = AWS.send $ AWS.getObject bucketName objectKey

getObjectForS3Uri :: MonadAWS m
  => S3Uri
  -> m AWS.GetObjectResponse
getObjectForS3Uri (S3Uri bucketName objectKey) = getObject bucketName objectKey

unsafeDownloadLbs :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m LBS.ByteString
unsafeDownloadLbs bucketName objectKey = do
  resp <- getObject bucketName objectKey
  lazyByteString (resp ^. AWS.gorsBody)

downloadLbs :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m (Maybe LBS.ByteString)
downloadLbs bucketName objectKey = do
  ebs <- (Right <$> unsafeDownloadLbs bucketName objectKey) `catch` \(err :: Error) -> case err of
    (ServiceError (ServiceError' _ (Status 404 _) _ _ _ _)) -> return (Left LBS.empty)
    _                                                       -> throwM err
  case ebs of
    Right bs -> return (Just bs)
    Left _   -> return Nothing

downloadLbsFromS3Uri :: (MonadAWS m, MonadResource m)
  => S3Uri
  -> m (Maybe LBS.ByteString)
downloadLbsFromS3Uri (S3Uri b k) = downloadLbs b k
