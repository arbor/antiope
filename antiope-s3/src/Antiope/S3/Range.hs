module Antiope.S3.Range
  ( Range(..)
  , rangeHeaderText
  ) where

import Antiope.S3.Types      (Range (Range))
import Data.Semigroup        ((<>))
import Network.AWS.Data.Text
import Prelude               hiding (last)

import qualified Antiope.Core.Internal.Show as S

rangeHeaderText :: Range -> Text
rangeHeaderText (Range first last) = "bytes=" <> S.tshowNum first <> "-" <> S.tshowNum last
