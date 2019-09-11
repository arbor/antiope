{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Antiope.S3
( s3ObjectSource
, putFile, putFile', putContent , putContent'
, copySingle
, fromS3Uri
, toS3Uri
, lsBucketResponseStream
, lsBucketStream
, lsEntries
, lsPrefix
, deleteFiles
, deleteFilesExcept
, fileExists
, Region(..)
, BucketName(..)
, ObjectKey(..)
, ETag(..)
, S3Uri(..)
) where

import Antiope.Core.Error           (handle404ToNone)
import Antiope.S3.Internal
import Antiope.S3.Types             (S3Uri (S3Uri, objectKey))
import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.AWS      hiding (send)
import Control.Monad.Trans.Resource
import Data.Conduit.List            (consume, unfoldM)
import Data.Maybe                   (catMaybes, isJust)
import Data.Monoid                  ((<>))
import Data.Text                    as T (Text, pack, unpack)
import Network.AWS                  (MonadAWS)
import Network.AWS.Data.Body        (_streamBody)
import Network.AWS.Data.Text        (toText)
import Network.AWS.S3
import Network.URI                  (URI (..), URIAuth (..), parseURI, unEscapeString)

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Conduit.Combinators as CC
import qualified Data.List                as List
import qualified Network.AWS              as AWS

chunkSize :: ChunkSize
chunkSize = ChunkSize (1024 * 1024)

type Prefix = Text

fromS3Uri :: Text -> Maybe S3Uri
fromS3Uri uri = do
  puri <- parseURI (unpack uri)
  auth <- puri & uriAuthority
  let b = pack $ auth & uriRegName       -- URI lib is pretty weird
  let k = pack $ unEscapeString $ drop 1 $ puri & uriPath
  pure $ S3Uri (BucketName b) (ObjectKey k)

s3ObjectSource :: (MonadAWS m, MonadResource m)
  => BucketName
  -> ObjectKey
  -> m (ConduitT () BS.ByteString m ())
s3ObjectSource bkt obj = do
  resp <- AWS.send $ getObject bkt obj
  return $ transPipe liftResourceT $ _streamBody $ resp ^. gorsBody

-- | Puts file into a specified S3 bucket
putFile :: MonadAWS m
  => BucketName       -- ^ Target bucket
  -> ObjectKey        -- ^ File name on S3
  -> FilePath         -- ^ Source file path
  -> m (Maybe ETag)   -- ^ Etag when the operation is successful
putFile b k f = do
  req <- chunkedFile chunkSize f
  view porsETag <$> AWS.send (putObject b k req)

putFile' :: MonadAWS m
  => S3Uri            -- ^ S3 URI
  -> FilePath         -- ^ Source file path
  -> m (Maybe ETag)   -- ^ Etag when the operation is successful
putFile' (S3Uri b k) f = do
  req <- chunkedFile chunkSize f
  view porsETag <$> AWS.send (putObject b k req)

putContent :: MonadAWS m
  => BucketName
  -> ObjectKey
  -> LBS.ByteString
  -> m (Maybe ETag)
putContent b k c = view porsETag <$> AWS.send (putObject b k (toBody c))

putContent' :: MonadAWS m
  => S3Uri
  -> LBS.ByteString
  -> m (Maybe ETag)
putContent' (S3Uri b k) = putContent b k

-- | Copies a single object within S3
copySingle :: MonadAWS m
  => BucketName          -- ^ Source bucket name
  -> ObjectKey           -- ^ Source key
  -> BucketName          -- ^ Target bucket name
  -> ObjectKey           -- ^ Target key
  -> m ()
copySingle sb sk tb tk = void . AWS.send $ copyObject tb (toText sb <> "/" <> toText sk) tk
     & coMetadataDirective ?~ MDCopy

-- Private --

-- Builds the request for the next page of a NextObjectsV2 request,
-- based on the original request and the most recent response.
nextPageReq :: ListObjectsV2 -> ListObjectsV2Response -> ListObjectsV2
nextPageReq initial resp =
  initial & lovContinuationToken .~ resp ^. lovrsNextContinuationToken

-- The type signature is like this so that it can be used with `unfoldM`
lsBucketPage :: MonadAWS m
  => Maybe ListObjectsV2
  -> m (Maybe (ListObjectsV2Response, Maybe ListObjectsV2))
lsBucketPage Nothing    = pure Nothing
lsBucketPage (Just req) = do
  resp <- AWS.send req
  pure . Just . (resp, ) $
    case resp ^. lovrsIsTruncated of
      Just True -> Just $ nextPageReq req resp
      _         -> Nothing

-- | Streams all pages of the result (ListObjectsV2Responses) of a ListObjectsV2
-- request from S3.
-- lsBucketResponseStream :: MonadAWS m => ListObjectsV2 -> ConduitT i ListObjectsV2Response m ()
lsBucketResponseStream :: MonadAWS m
  => ListObjectsV2
  -> ConduitM a ListObjectsV2Response m ()
lsBucketResponseStream bar = unfoldM lsBucketPage (Just bar)

-- | Streams all Objects from all pages of the result of a ListObjectsV2
-- request from S3.
-- lsBucketStream :: MonadAWS m => ListObjectsV2 -> ConduitT i Object m ()
lsBucketStream :: MonadAWS m
  => ListObjectsV2
  -> ConduitM a Object m ()
lsBucketStream bar = lsBucketResponseStream bar .| concatMapC (^. lovrsContents)

-- | Lists the specified prefix in a bucket, recursively.
lsPrefix :: MonadAWS m
  => BucketName
  -> Prefix
  -> m [S3Uri]
lsPrefix b p =
  runConduit $
    lsBucketStream (listObjectsV2 b & lovPrefix ?~ p)
    .| mapC (S3Uri b . view oKey)
    .| sinkList

-- | Lists the specified objects in a bucket, non-recursively.
lsEntries :: MonadAWS m
  => BucketName
  -> ObjectKey
  -> m [ObjectKey]
lsEntries bkt dir =
  let req = listObjectsV2 bkt & lovPrefix ?~ toText dir & lovDelimiter ?~ ('/' :: Delimiter)
  in runConduit $ lsBucketResponseStream req .| CC.concatMap lsResponseToObjectsList .| consume
  where
    lsResponseToObjectsList rs = keys <> prefixes
      where
        keys = rs ^. lovrsContents <&> (^. oKey)
        maybePrefixes = rs ^. lovrsCommonPrefixes <&> (^. cpPrefix)
        prefixes = ObjectKey <$> catMaybes maybePrefixes

-- | Deletes specified keys in a bucket.
-- Returns a list of keys that were successfully deleted.
--
-- Will fail monadically (using 'fail') if the response indicates any errors.
deleteFiles :: MonadAWS m
  => BucketName
  -> [ObjectKey]
  -> m [S3Uri]
deleteFiles b ks = do
  let dObjs = delete' & dObjects .~ (objectIdentifier <$> ks)
  resp <- AWS.send (deleteObjects b dObjs)
  unless (List.null $ resp ^. drsErrors) $
    fail (resp ^. drsErrors & show)
  let deleted = resp ^.. drsDeleted . each . dKey & catMaybes <&> S3Uri b
  pure deleted

-- | Deletes all the keys in a specified prefix EXCEPT the specified ones.
-- Returns a list of objects that were successfully deleted.
deleteFilesExcept :: MonadAWS m
  => BucketName
  -> Prefix
  -> [ObjectKey]
  -> m [S3Uri]
deleteFilesExcept b p uris = do
  existing <- lsPrefix b p
  case (objectKey <$> existing) List.\\ uris of
    [] -> pure []
    xs -> deleteFiles b xs

-- | Checks if the file exists on S3
fileExists :: MonadAWS m
  => S3Uri
  -> m Bool
fileExists (S3Uri b k) =
  isJust <$> handle404ToNone (AWS.send (headObject b k))

