{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Antiope.S3.Lazy
  ( unsafeDownload
  , unsafeDownloadRequest
  , download
  , downloadRequest
  , downloadFromS3Uri
  , listObjectsV2
  , listS3Uris
  , listObjectsV2DList
  , listS3UrisDList
  , s3UriToListObjectsV2
  ) where

import Antiope.Core.Error
import Antiope.S3.GetObject
import Antiope.S3.Types             (S3Uri (S3Uri), s3UriToListObjectsV2)
import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Network.AWS                  (MonadAWS)

import qualified Antiope.S3.Internal  as I
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList           as DL
import qualified Network.AWS          as AWS
import qualified Network.AWS.S3       as AWS
import qualified System.IO.Unsafe     as IO

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

listObjectsV2DList :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m (DL.DList AWS.ListObjectsV2Response)
listObjectsV2DList req = do
  f <- askUnliftIO
  r <- AWS.send req
  case r ^. AWS.lovrsIsTruncated of
    Just True -> do
      rs <- liftIO $ IO.unsafeInterleaveIO (unliftIO f (listObjectsV2DList (I.nextPageReq req r)))
      return (DL.cons r rs)
    _ -> return (DL.singleton r)

listS3UrisDList :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m (DL.DList S3Uri)
listS3UrisDList req = listObjectsV2DList req >>= toS3Uris
  where toS3Uris responses  = return (responses >>= toS3Uris')
        toS3Uris' response  = do
          c <- response ^. AWS.lovrsContents & DL.fromList
          response ^.. AWS.lovrsName . _Just & DL.fromList <&> \bucketName -> S3Uri bucketName (c ^. AWS.oKey)

listObjectsV2 :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m [AWS.ListObjectsV2Response]
listObjectsV2 req = DL.toList <$> listObjectsV2DList req

listS3Uris :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m [S3Uri]
listS3Uris req = DL.toList <$> listS3UrisDList req
