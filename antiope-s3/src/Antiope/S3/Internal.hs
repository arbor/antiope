{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Antiope.S3.Internal where

import Control.Lens
import Data.Monoid    ((<>))
import Data.Text      (Text)
import Network.AWS    (MonadAWS)
import Network.AWS.S3 (BucketName (..), ObjectKey (..))
import Network.AWS.S3

import qualified Network.AWS as AWS

toS3Uri :: BucketName -> ObjectKey -> Text
toS3Uri (BucketName b) (ObjectKey k) = "s3://" <> b <> "/" <> k

-- Builds the request for the next page of a NextObjectsV2 request,
-- based on the original request and the most recent response.
nextPageReq :: ListObjectsV2 -> ListObjectsV2Response -> ListObjectsV2
nextPageReq initial resp = initial & lovContinuationToken .~ resp ^. lovrsNextContinuationToken

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
