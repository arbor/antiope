{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Antiope.S3.Request
  ( getObjectUri
  ) where

import Antiope.S3.Types
import Control.Lens
import Data.Generics.Product.Any

import qualified Network.AWS.S3 as AWS

getObjectUri :: S3Uri -> AWS.GetObject
getObjectUri uri = AWS.getObject bucket objectKey
  where bucket    = uri ^. the @"bucket"
        objectKey = uri ^. the @"objectKey"
