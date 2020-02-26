{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Antiope.S3.Lazy
  ( unsafeDownload
  , unsafeDownloadRequest
  , unsafeDownloadIfModifiedSince
  , download
  , downloadRequest
  , downloadFromS3Uri
  , downloadIfModifiedSince
  , listObjectsV2
  , listS3Uris
  , dlistObjectsV2
  , dlistS3Uris
  , s3UriToListObjectsV2
  , DownloadResult (..)
  , S3Uri(..)
  , AWS.BucketName
  , AWS.ObjectKey
  , UTCTime
  ) where

import Antiope.Core.Error
import Antiope.S3.GetObject
import Antiope.S3.Types             (DownloadResult (..), S3Uri (S3Uri), s3UriToListObjectsV2)
import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Data.Maybe                   (fromMaybe)
import Data.Time.Clock              (UTCTime)
import Data.Time.Clock.POSIX        (posixSecondsToUTCTime)
import Network.AWS                  (MonadAWS)
import Network.HTTP.Types.Status    (Status (..))

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

unsafeDownloadIfModifiedSince :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> Maybe UTCTime
  -> m (UTCTime, LBS.ByteString)
unsafeDownloadIfModifiedSince bkt obj since = do
  let req = AWS.getObject bkt obj & AWS.goIfModifiedSince .~ since
  resp <- AWS.send req
  -- in practice AWS will never return Nothing for the timestamp, but the API allows it
  let modified = fromMaybe (posixSecondsToUTCTime 0) (resp ^. AWS.gorsLastModified)
  body <- lazyByteString (resp ^. AWS.gorsBody)
  pure (modified, body)

downloadIfModifiedSince :: (MonadAWS m, MonadResource m)
  => S3Uri
  -> Maybe UTCTime
  -> m (DownloadResult LBS.ByteString)
downloadIfModifiedSince uri@(S3Uri bucketName objectKey) since =
  handleServiceError
    (unsafeDownloadIfModifiedSince bucketName objectKey since)
    (\(ts, bs) -> Downloaded ts uri bs)
    (\case
        Status 404 _ -> Just $ NotFound uri
        Status 304 _ -> Just $ NotModified uri
        _            -> Nothing
    )

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

dlistObjectsV2 :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m (DL.DList AWS.ListObjectsV2Response)
dlistObjectsV2 req = do
  f <- askUnliftIO
  r <- AWS.send req
  case r ^. AWS.lovrsIsTruncated of
    Just True -> do
      rs <- liftIO $ IO.unsafeInterleaveIO (unliftIO f (dlistObjectsV2 (I.nextPageReq req r)))
      return (DL.cons r rs)
    _ -> return (DL.singleton r)

dlistS3Uris :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m (DL.DList S3Uri)
dlistS3Uris req = dlistObjectsV2 req >>= toS3Uris
  where toS3Uris responses  = return (responses >>= toS3Uris')
        toS3Uris' response  = do
          c <- response ^. AWS.lovrsContents & DL.fromList
          response ^.. AWS.lovrsName . _Just & DL.fromList <&> \bucketName -> S3Uri bucketName (c ^. AWS.oKey)

listObjectsV2 :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m [AWS.ListObjectsV2Response]
listObjectsV2 req = DL.toList <$> dlistObjectsV2 req

listS3Uris :: (MonadAWS m, MonadResource m, MonadUnliftIO m)
  => AWS.ListObjectsV2
  -> m [S3Uri]
listS3Uris req = DL.toList <$> dlistS3Uris req
