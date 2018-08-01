{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Antiope.S3.Types
  ( X.BucketName(..)
  , X.ObjectKey(..)
  , X.ETag(..)
  , S3Uri(..)
  ) where

import Antiope.S3.Internal
import Control.Lens
import Control.Monad.Logger      (ToLogStr (..))
import Data.Generics.Product.Any
import Data.String               (fromString)
import GHC.Generics
import Network.AWS.Data
import Network.AWS.S3            (BucketName (..), ObjectKey (..))

import qualified Data.Text            as T
import qualified Network.AWS.S3.Types as X

data S3Uri = S3Uri
  { bucket    :: BucketName
  , objectKey :: ObjectKey
  } deriving (Show, Eq, Generic)

instance ToText S3Uri where
  toText loc = toS3Uri (loc ^. the @"bucket") (loc ^. the @"objectKey")

instance ToLogStr S3Uri where
  toLogStr s = fromString $ T.unpack $ toText s
