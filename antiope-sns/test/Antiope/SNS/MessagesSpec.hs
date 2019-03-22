module Antiope.SNS.MessagesSpec
where

import Antiope.SNS.Messages
import Data.Aeson
import Data.Time.Calendar
import Data.Time.Clock

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as Gen
import Hedgehog.Range              as Range
import Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "Antiope.SNS.MessagesSpec" $ do
  it "Can encode and decode SnsMessage" $ require $ property $ do
    body <- forAll $ simpleJson
    msg  <- forAll $ snsMessageGen body
    tripping msg encode decode

simpleJson :: MonadGen m => m Value
simpleJson = do
  txtVal <- Gen.text (Range.linear 1 25) Gen.alphaNum
  numVal <- Gen.double (Range.linearFrac (-32000.0) 32000.0)
  pure $ object [ "text" .= txtVal, "num" .= numVal ]

snsMessageGen :: MonadGen m => a -> m (SnsMessage a)
snsMessageGen a = do
  let txtGen = Gen.text (Range.linear 1 25) Gen.alphaNum
  SnsMessage
    <$> Gen.maybe txtGen
    <*> Gen.maybe txtGen
    <*> Gen.maybe txtGen
    <*> Gen.maybe txtGen
    <*> Gen.maybe genUTCTime
    <*> Gen.constant a

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86401)
  let diff = secondsToDiffTime secs
  pure $ UTCTime day diff
