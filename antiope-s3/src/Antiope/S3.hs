{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Antiope.S3
( downloadLBS
, downloadLBS'
, downloadS3Uri
, s3ObjectSource
, putFile, putContent , putContent'
, copySingle
, fromS3Uri
, toS3Uri
, lsBucketStream
, BucketName(..)
, ObjectKey(..)
, ETag(..)
, S3Uri(..)
) where

import Antiope.S3.Internal
import Antiope.S3.Types             (S3Uri (S3Uri))
import Control.Lens
import Control.Monad
import Control.Monad.Catch          (MonadCatch, catch)
import Control.Monad.Trans.AWS      hiding (send)
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy         (ByteString, empty)
import Data.Conduit
import Data.Conduit.Binary          (sinkLbs)
import Data.Conduit.Combinators     as CC (concatMap)
import Data.Conduit.List            (unfoldM)
import Data.Monoid                  ((<>))
import Data.Text                    (Text, pack, unpack)
import Network.AWS                  (Error (..), ServiceError (..))
import Network.AWS.Data
import Network.AWS.Data.Body        (_streamBody)
import Network.AWS.S3
import Network.HTTP.Types.Status    (Status (..))
import Network.URI                  (URI (..), URIAuth (..), parseURI)

import qualified Data.ByteString as BS
import qualified Network.AWS     as AWS

chunkSize :: ChunkSize
chunkSize = ChunkSize (1024 * 1024)

fromS3Uri :: Text -> Maybe S3Uri
fromS3Uri uri = do
  puri <- parseURI (unpack uri)
  auth <- puri & uriAuthority
  let b = pack $ auth & uriRegName       -- URI lib is pretty weird
  let k = pack $ drop 1 $ puri & uriPath
  pure $ S3Uri (BucketName b) (ObjectKey k)

downloadLBS :: (MonadResource m, HasEnv e)
  => e
  -> BucketName
  -> ObjectKey
  -> m ByteString
downloadLBS e bucketName objectKey = AWS.runAWS e $ do
  resp <- AWS.send $ getObject bucketName objectKey
  (resp ^. gorsBody) `sinkBody` sinkLbs

downloadLBS' :: (MonadResource m, MonadCatch m, HasEnv e)
  => e
  -> BucketName
  -> ObjectKey
  -> m (Maybe ByteString)
downloadLBS' e bucketName objectKey = do
  ebs <- (Right <$> downloadLBS e bucketName objectKey) `catch` \(err :: Error) -> case err of
    (ServiceError (ServiceError' _ (Status 404 _) _ _ _ _)) -> return (Left empty)
    _                                                       -> throwM err
  case ebs of
    Right bs -> return (Just bs)
    Left _   -> return Nothing

downloadS3Uri :: (MonadResource m, MonadCatch m, HasEnv e)
  => e
  -> S3Uri
  -> m (Maybe ByteString)
downloadS3Uri e (S3Uri b k) = downloadLBS' e b k

s3ObjectSource :: (MonadResource m, HasEnv e)
  => e
  -> BucketName
  -> ObjectKey
  -> m (ConduitT () BS.ByteString m ())
s3ObjectSource e bkt obj = do
  resp <- AWS.runAWS e $ AWS.send $ getObject bkt obj
  return $ transPipe liftResourceT $ _streamBody $ resp ^. gorsBody

-- | Puts file into a specified S3 bucket
putFile :: (HasEnv e, MonadUnliftIO m)
  => e
  -> BucketName       -- ^ Target bucket
  -> ObjectKey        -- ^ File name on S3
  -> FilePath         -- ^ Source file path
  -> m (Maybe ETag)   -- ^ Etag when the operation is successful
putFile e b k f = AWS.runResourceT . AWS.runAWS e $ do
    req <- chunkedFile chunkSize f
    view porsETag <$> AWS.send (putObject b k req)

putContent :: (MonadUnliftIO m, HasEnv e)
  => e
  -> BucketName
  -> ObjectKey
  -> ByteString
  -> m (Maybe ETag)
putContent e b k c = AWS.runResourceT . AWS.runAWS e $
  view porsETag <$> AWS.send (putObject b k (toBody c))

putContent' :: (MonadUnliftIO m, HasEnv e)
  => e
  -> S3Uri
  -> ByteString
  -> m (Maybe ETag)
putContent' e (S3Uri b k) = putContent e b k

-- | Copies a single object within S3
copySingle :: (MonadUnliftIO m, HasEnv e)
  => e
  -> BucketName          -- ^ Source bucket name
  -> ObjectKey           -- ^ Source key
  -> BucketName          -- ^ Target bucket name
  -> ObjectKey           -- ^ Target key
  -> m ()
copySingle e sb sk tb tk = AWS.runResourceT . AWS.runAWS e $
  void . AWS.send $ copyObject tb (toText sb <> "/" <> toText sk) tk
     & coMetadataDirective ?~ MDCopy

-- Private --

-- Builds the request for the next page of a NextObjectsV2 request,
-- based on the original request and the most recent response.
nextPageReq :: ListObjectsV2 -> ListObjectsV2Response -> ListObjectsV2
nextPageReq initial resp =
  initial & lovContinuationToken .~ resp ^. lovrsNextContinuationToken

-- The type signature is like this so that it can be used with `unfoldM`
lsBucketPage :: (MonadUnliftIO m, HasEnv e)
  => e
  -> Maybe ListObjectsV2
  -> m (Maybe (ListObjectsV2Response, Maybe ListObjectsV2))
lsBucketPage _ Nothing    = pure Nothing
lsBucketPage e (Just req) = AWS.runResourceT . AWS.runAWS e $ do
  resp <- AWS.send req
  pure . Just . (resp, ) $
    case resp ^. lovrsIsTruncated of
      Just True -> Just $ nextPageReq req resp
      _         -> Nothing

-- | Streams the entire set of results (i.e. all pages) of a ListObjectsV2
-- request from S3.
-- lsBucketStream :: MonadAWS m => ListObjectsV2 -> ConduitT i Object m ()
lsBucketStream :: (HasEnv e, MonadUnliftIO m)
  => e
  -> ListObjectsV2
  -> ConduitM a Object m ()
lsBucketStream e bar = unfoldM (lsBucketPage e) (Just bar) .| CC.concatMap (^. lovrsContents)
