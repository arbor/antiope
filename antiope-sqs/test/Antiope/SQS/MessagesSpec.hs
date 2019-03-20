module Antiope.SQS.MessagesSpec
where

import Antiope.SQS.Messages
import Data.Aeson

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as Gen
import Hedgehog.Range              as Range
import Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "Antiope.SQS.MessagesSpec" $ do
  it "Implement me" $ require $ property $ do
    True === True
