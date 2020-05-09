{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Antiope.Messages.MessagesSpec
  ( spec
  ) where

import Antiope.Messages
import Data.Aeson                  (decode, encode, (.=))
import Data.Scientific             (fromFloatDigits)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as Gen
import Hedgehog.Range              as Range
import Test.Hspec

import qualified Data.Aeson as Aeson

{- HLINT ignore "Redundant do"    -}

jsonObject :: MonadGen m => m Aeson.Value
jsonObject = do
  fields  <- Gen.list (Range.linear 0 10) jsonField
  kvs     <- traverse (\k -> (\v -> k .= v) <$> jsonValue) fields
  pure $ Aeson.object kvs
  where
    jsonValue   = Gen.choice [jsonString, jsonNumber, jsonBool, jsonNull]
    jsonField   = Gen.text (Range.linear 0 10) Gen.alphaNum
    jsonString  = Aeson.String <$> Gen.text (Range.linear 0 50) Gen.alphaNum
    jsonNumber  = (Aeson.Number . fromFloatDigits) <$> Gen.double (Range.linearFrac (-32000.0) 32000.0)
    jsonBool    = Aeson.Bool <$> Gen.bool
    jsonNull    = pure Aeson.Null

spec :: Spec
spec = describe "Antiope.Core.MessagesSpec" $ do
  it "Can encode and decode With" $ require $ property $ do
    obj <- forAll $ jsonObject
    tripping (With @"payload" obj) encode decode

  it "Can encode and decode inner-encoded" $ require $ property $ do
    obj <- forAll $ jsonObject
    tripping (WithEncoded @"payload" obj) encode decode
