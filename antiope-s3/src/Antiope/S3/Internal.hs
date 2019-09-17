{-# LANGUAGE OverloadedStrings #-}

module Antiope.S3.Internal where

import Data.Monoid    ((<>))
import Data.Text      (Text)
import Network.AWS.S3 (BucketName (..), ObjectKey (..))

toS3Uri :: BucketName -> ObjectKey -> Text
toS3Uri (BucketName b) (ObjectKey k) = "s3://" <> b <> "/" <> k
