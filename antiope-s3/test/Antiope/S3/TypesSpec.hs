module Antiope.S3.TypesSpec (spec) where

import Antiope.Core
import Antiope.S3.Types
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "Antiope.TypesSpec" $ do
  it "Can parse S3Uri" $ requireTest $ do
    fromText ""                 === (Left "not enough input"  :: Either String S3Uri)
    fromText "boo"              === (Left "string"            :: Either String S3Uri)
    fromText "s3://hello"       === Right (S3Uri (BucketName "hello") (ObjectKey ""))
    fromText "s3://hello/"      === Right (S3Uri (BucketName "hello") (ObjectKey ""))
    fromText "s3://hello/a"     === Right (S3Uri (BucketName "hello") (ObjectKey "a"))
    fromText "s3://hello/ab"    === Right (S3Uri (BucketName "hello") (ObjectKey "ab"))
    fromText "s3://hello/ab "   === Right (S3Uri (BucketName "hello") (ObjectKey "ab "))
    fromText "s3://hello/ab c"  === Right (S3Uri (BucketName "hello") (ObjectKey "ab c"))
