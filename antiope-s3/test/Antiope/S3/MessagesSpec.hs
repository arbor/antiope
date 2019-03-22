module Antiope.S3.MessagesSpec
where

import Antiope.S3          (BucketName (..), ETag (..), ObjectKey (..))
import Antiope.S3.Messages
import Data.Aeson          (decode, encode)
import Data.Monoid         ((<>))
import Data.Text           (Text, pack)
import Data.Time.Calendar
import Data.Time.Clock

import qualified Data.Text as Text

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as Gen
import Hedgehog.Range              as Range
import Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2019)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    secs <- toInteger <$> Gen.int (Range.constant 0 86401)
    let diff = secondsToDiffTime secs
    pure $ UTCTime day diff

datedPath :: MonadGen m => m Text
datedPath = do
  year  <- Gen.int (Range.linear 2000 2050)
  month <- Gen.int (Range.linear 1 12)
  day   <- Gen.int (Range.linear 1 28)
  let parts = zipWith (\a b -> a <> "=" <> pack (show b)) ["year", "month", "day"] [year, month, day]
  pure $ Text.intercalate "/" parts

randomPath :: MonadGen m => m Text
randomPath = do
  parts <- Gen.list (Range.linear 1 5) (Gen.text (Range.linear 1 10) Gen.alphaNum)
  pure $ Text.intercalate "/" parts

s3Message :: MonadGen m => m S3Message
s3Message = do
  eName <- Gen.text (Range.linear 1 10) Gen.alphaNum
  eType <- Gen.text (Range.linear 1 10) Gen.alphaNum
  time  <- Gen.maybe genUTCTime
  bkt   <- Gen.text (Range.linear 1 20) Gen.alphaNum
  file  <- Gen.text (Range.linear 1 20) Gen.alphaNum
  path  <- Gen.choice [datedPath, randomPath]
  sz    <- Gen.int64 (Range.linear 0 maxBound)
  etag  <- Gen.maybe (Gen.utf8 (Range.singleton 10) Gen.alphaNum)
  pure S3Message
    { eventTime = time
    , eventName = EventName eType eName
    , bucket    = BucketName bkt
    , key       = ObjectKey (path <> "/" <> file)
    , size      = sz
    , eTag      = fmap ETag etag
    }

spec :: Spec
spec = describe "Antiope.S3.MessagesSpec" $ do
  it "Can encode and decode S3Message" $ require $ property $ do
    msg <- forAll $ s3Message
    tripping msg encode decode
