{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.S3.GetObject
  ( lazyByteString
  , getObject
  , getObjectForS3Uri
  ) where

import Antiope.S3.Types             (S3Uri (S3Uri))
import Control.Monad.Trans.AWS      hiding (send)
import Control.Monad.Trans.Resource
import Data.Conduit.Lazy            (lazyConsume)
import Network.AWS                  (MonadAWS)
import Network.AWS.Data.Body        (_streamBody)

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
