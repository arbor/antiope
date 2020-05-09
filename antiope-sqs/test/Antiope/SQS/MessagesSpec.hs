module Antiope.SQS.MessagesSpec
  ( spec
  ) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Redundant do"    -}

spec :: Spec
spec = describe "Antiope.SQS.MessagesSpec" $ do
  it "Implement me" $ require $ property $ do
    True === True
