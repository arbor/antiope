module Antiope.SQS.MessagesSpec
where

import Antiope.SQS.Messages
import Data.Aeson

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as Gen
import Hedgehog.Range              as Range
import Test.Hspec

import Debug.Trace

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "Antiope.SQS.MessagesSpec" $ do
  it "Can encode and decode SqsMessage" $ require $ property $ do
    body <- forAll $ simpleJson
    msg  <- forAll $ sqsMessageGen body
    tripping (traceShowId msg) encode decode

simpleJson :: MonadGen m => m Value
simpleJson = do
  txtVal <- Gen.text (Range.linear 1 25) Gen.alphaNum
  numVal <- Gen.double (Range.linearFrac (-32000.0) 32000.0)
  pure $ object [ "text" .= txtVal, "num" .= numVal ]

sqsMessageGen :: (FromJSON a, ToJSON a, MonadGen m) => a -> m (SqsMessage a)
sqsMessageGen a = do
  let txtGen = Gen.text (Range.linear 1 25) Gen.alphaNum
  SqsMessage
    <$> Gen.maybe txtGen
    <*> Gen.maybe txtGen
    <*> Gen.maybe txtGen
    <*> Gen.maybe txtGen
    <*> Gen.maybe (Gen.constant a)
