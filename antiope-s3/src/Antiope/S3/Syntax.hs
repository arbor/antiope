{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Antiope.S3.Syntax
  ( (</>)
  ) where

import Antiope.S3.Types
import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Data.Text                 (Text)

-- | Append text to an S3Uri
(</>) :: S3Uri -> Text -> S3Uri
(</>) uri suffix = uri & the @"objectKey" . the @1 %~ (<> ("/" <> suffix))
