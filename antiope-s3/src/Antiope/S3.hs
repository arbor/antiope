{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.S3
( downloadLBS
, downloadLBS'
, s3ObjectSource
, putFile, putContent
, copySingle
, toS3Uri
, Region(..)
, BucketName(..)
, ObjectKey(..)
, ETag(..)
, S3Location(..)
, MonadAWS
, MonadResource
, FromText(..), fromText
, ToText(..)
, module Network.AWS.S3
) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch          (catch)
import           Control.Monad.Morph          (hoist)
import           Control.Monad.Trans.AWS      hiding (send)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.ByteString.Lazy         (ByteString, empty)
import           Data.Conduit
import           Data.Conduit.Binary          (sinkLbs)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Network.AWS                  (Error (..), MonadAWS,
                                               ServiceError (..), send)
import           Network.AWS.Data
import           Network.AWS.Data.Body        (_streamBody)
import           Network.AWS.S3
import           Network.HTTP.Types.Status    (Status (..))


chunkSize :: ChunkSize
chunkSize = ChunkSize (1024*1024)

data S3Location = S3Location
  { s3Bucket    :: BucketName
  , s3ObjectKey :: ObjectKey
  } deriving (Show, Eq)

instance ToText S3Location where
  toText loc = toS3Uri (s3Bucket loc) (s3ObjectKey loc)

toS3Uri :: BucketName -> ObjectKey -> Text
toS3Uri (BucketName b) (ObjectKey k) =
  "s3://" <> b <> "/" <> k

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

downloadS3File' :: (MonadResource m, MonadAWS m)
             => BucketName
             -> ObjectKey
             -> m (ResumableSource m BS.ByteString)
downloadS3File' bkt obj = do
  resp <- send $ getObject bkt obj
  return $ hoist liftResourceT $ _streamBody $ resp ^. gorsBody

s3ObjectSource :: (MonadResource m, MonadAWS m)
                 => BucketName
                 -> ObjectKey
                 -> m (Source m BS.ByteString)
s3ObjectSource bkt obj =
  fst <$> (downloadS3File' bkt obj >>= unwrapResumable)

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
