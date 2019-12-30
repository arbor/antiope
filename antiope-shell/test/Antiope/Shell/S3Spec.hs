module Antiope.Shell.S3Spec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "Antiope.S3.S3Spec" $ do
  it "Test stub" $ requireTest $ do
    True === True


