{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.S3
( downloadLBS
, downloadLBS'
, putFile, putContent
, copySingle
, BucketName(..)
, ObjectKey(..)
, ETag(..)
, s3UriString
, MonadAWS
, MonadResource
) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch          (catch)
import           Control.Monad.Trans.AWS      hiding (send)
import           Control.Monad.Trans.Resource
import           Data.ByteString.Lazy         (ByteString, empty)
import           Data.Conduit.Binary          (sinkLbs)
import           Data.Monoid                  ((<>))
import           Data.Text                    (unpack)
import           Network.AWS                  (Error (..), MonadAWS,
                                               ServiceError (..), send)
import           Network.AWS.Data
import           Network.AWS.S3
import           Network.HTTP.Types.Status    (Status (..))

chunkSize :: ChunkSize
chunkSize = ChunkSize (1024*1024)

downloadLBS :: (MonadResource m, MonadAWS m)
            => BucketName
            -> ObjectKey
            -> m ByteString
downloadLBS bucketName objectKey = do
  resp <- send $ getObject bucketName objectKey
  (resp ^. gorsBody) `sinkBody` sinkLbs

downloadLBS' :: (MonadResource m, MonadAWS m)
            => BucketName
            -> ObjectKey
            -> m (Maybe ByteString)
downloadLBS' bucketName objectKey = do
  ebs <- (Right <$> downloadLBS bucketName objectKey) `catch` \(e :: Error) -> case e of
    (ServiceError (ServiceError' _ (Status 404 _) _ _ _ _)) -> return (Left empty)
    _                                                       -> throwM e
  case ebs of
    Right bs -> return (Just bs)
    Left _   -> return Nothing

s3UriString :: BucketName -> ObjectKey -> String
s3UriString (BucketName b) (ObjectKey k) =
  unpack $ "s3://" <> b <> "/" <> k

-- | Puts file into a specified S3 bucket
putFile :: MonadAWS m
        => BucketName       -- ^ Target bucket
        -> ObjectKey        -- ^ File name on S3
        -> FilePath         -- ^ Source file path
        -> m (Maybe ETag)   -- ^ Etag when the operation is successful
putFile b k f = do
    req <- chunkedFile chunkSize f
    view porsETag <$> send (putObject b k req)

putContent :: MonadAWS m
           => BucketName
           -> ObjectKey
           -> ByteString
           -> m (Maybe ETag)
putContent b k c =
  view porsETag <$> send (putObject b k (toBody c))

-- | Copies a single object within S3
copySingle :: MonadAWS m
           => BucketName          -- ^ Source bucket name
           -> ObjectKey           -- ^ Source key
           -> BucketName          -- ^ Target bucket name
           -> ObjectKey           -- ^ Target key
           -> m ()
copySingle sb sk tb tk =
  void . send $ copyObject tb (toText sb <> "/" <> toText sk) tk
     & coMetadataDirective ?~ MDCopy
