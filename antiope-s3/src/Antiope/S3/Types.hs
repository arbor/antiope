{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Antiope.S3.Types
  ( X.BucketName(..)
  , X.ObjectKey(..)
  , X.ETag(..)
  , S3Uri(..)
  , readBucketName
  , readWhile
  ) where

import Antiope.S3.Internal
import Control.Lens
import Control.Monad
import Control.Monad.Logger      (ToLogStr (..))
import Data.Char
import Data.Generics.Product.Any
import Data.List
import Data.String               (fromString)
import GHC.Generics
import Network.AWS.Data
import Network.AWS.S3            (BucketName (..), ObjectKey (..))

import qualified Data.Text                       as T
import qualified Network.AWS.S3.Types            as X
import qualified Text.ParserCombinators.ReadPrec as RP

data S3Uri = S3Uri
  { bucket    :: BucketName
  , objectKey :: ObjectKey
  } deriving (Show, Eq, Generic)

instance ToText S3Uri where
  toText loc = toS3Uri (loc ^. the @"bucket") (loc ^. the @"objectKey")

instance ToLogStr S3Uri where
  toLogStr s = fromString $ T.unpack $ toText s

readString :: String -> RP.ReadPrec String
readString s = do
  remainder <- RP.look
  if s `isPrefixOf` remainder
    then do
      replicateM_ (length s) RP.get
      return s
    else RP.pfail

readWhile :: (Char -> Bool) -> RP.ReadPrec String
readWhile f = do
  remainder <- RP.look
  let taken = takeWhile f remainder
  replicateM_ (length taken) RP.get
  return taken

-- As per: https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-s3-bucket-naming-requirements.html
readBucketName :: RP.ReadPrec BucketName
readBucketName = do
  bucketName <- readWhile bucketNameChar
  when (length bucketName < 3 || length bucketName > 63) RP.pfail
  return (BucketName (T.pack bucketName))
  where bucketNameChar c = isLower c || isDigit c || c == '.' || c == '-'

-- As per: https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html
readObjectKey :: RP.ReadPrec ObjectKey
readObjectKey = do
  objKey <- readWhile (/= ' ')
  when (length objKey <= 1 || length objKey > 1025) RP.pfail
  return (ObjectKey (T.pack $ drop 1 objKey))

instance Read S3Uri where
  readsPrec = RP.readPrec_to_S $ do
    _  <- readString "s3://"
    bn <- readBucketName
    S3Uri bn <$> readObjectKey
