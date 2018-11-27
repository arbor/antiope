module Antiope.Core.Internal.Show
  ( tshowNum
  ) where

import Data.Text (Text)

import qualified Data.Text as T

tshowNum :: (Num a, Show a) => a -> Text
tshowNum = T.pack . show
