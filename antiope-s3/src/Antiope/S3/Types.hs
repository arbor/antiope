{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Antiope.S3.Types
  ( S3.BucketName(..)
  , S3.ObjectKey(..)
  , S3.ETag(..)
  , S3Uri(..)
  , readBucketName
  , readWhile
  , dirname
  , Range(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Generics.Product.Any
import Data.List
import Data.Semigroup            ((<>))
import GHC.Generics
import Network.AWS.Data
import Network.AWS.S3            (BucketName (BucketName), ObjectKey (ObjectKey))
import Network.URI               (unEscapeString)

import qualified Antiope.S3.Internal             as S3
import qualified Data.Aeson                      as J
import qualified Data.Aeson.Types                as J
import qualified Data.Attoparsec.Combinator      as DAC
import qualified Data.Attoparsec.Text            as DAT
import qualified Data.Text                       as T
import qualified Network.AWS.S3.Types            as S3
import qualified Text.ParserCombinators.ReadPrec as RP

data S3Uri = S3Uri
  { bucket    :: BucketName
  , objectKey :: ObjectKey
  } deriving (Show, Eq, Ord, Generic)

instance FromText S3Uri where
  parser = do
    _  <- DAT.string "s3://"
    bn <- BucketName . T.pack <$> DAC.many1 (DAT.satisfy (\c -> c /= '/' && c /= ' '))
    _  <- optional (DAT.char '/')
    ok <- ObjectKey . T.pack <$> many DAT.anyChar
    DAT.endOfInput
    return (S3Uri bn ok)

instance ToText S3Uri where
  toText loc = S3.toS3Uri (loc ^. the @"bucket") (loc ^. the @"objectKey")

instance ToJSON S3Uri where
  toJSON s3Uri = J.String (toText s3Uri)

instance FromJSON S3Uri where
  parseJSON v = case v of
    J.String s -> case fromText s of
      Right s3Uri -> return s3Uri
      Left msg    -> J.typeMismatch ("S3Uri (" <> msg <> ")") v
    _ -> J.typeMismatch "S3Uri" v

data Range = Range
  { first :: Int
  , last  :: Int
  } deriving (Eq, Show, Generic)

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

instance Read S3Uri where
  readsPrec = RP.readPrec_to_S $ do
    _  <- readString "s3://"
    bn <- readBucketName
    ok <- ObjectKey . T.pack . unEscapeString . drop 1 <$> readWhile (/= ' ')
    return (S3Uri bn ok)

dirname :: S3Uri -> S3Uri
dirname (S3Uri bk (ObjectKey key)) = S3Uri bk (ObjectKey newKey)
  where newKey = T.intercalate "/" (reverse (drop 1 (dropWhile T.null (reverse (T.splitOn "/" key)))))
