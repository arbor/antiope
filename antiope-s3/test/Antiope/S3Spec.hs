module Antiope.S3Spec (spec) where

import Antiope.Core
import Antiope.Env
import Control.Monad.IO.Class
import Data.Maybe                  (fromJust)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.Environment          (lookupEnv)
import Test.Hspec

import qualified Antiope.S3.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "Antiope.S3Spec" $ do
  it "downloadS3Uri" $ requireTest $ do
    maybeRemoteS3Uri <- liftIO (lookupEnv "REMOTE_S3_URI")
    case maybeRemoteS3Uri of
      Just remoteS3Uri -> do
        env <- liftIO $ mkEnv Oregon (const LBS.putStrLn)
        liftIO $ runResourceT $ do
          mlbs <- runAws env $ LBS.downloadFromS3Uri (read remoteS3Uri)
          let lbs = fromJust mlbs
          let !_ = LBS.take 10 lbs
          return ()
        True === True
      Nothing -> True === True
