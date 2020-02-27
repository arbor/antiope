{-# LANGUAGE OverloadedStrings #-}
module Antiope.Env
  ( mkEnv
  , AWS.Env
  , AWS.HasEnv(..)
  , AWS.LogLevel(..)
  , AWS.Region(..)
  ) where

import Control.Lens
import Control.Monad.Trans.AWS hiding (LogLevel)
import Data.ByteString.Builder
import Network.HTTP.Client     (HttpException (..), HttpExceptionContent (..))

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Network.AWS                   as AWS

mkEnv :: Region -> (AWS.LogLevel -> LBS.ByteString -> IO ()) -> IO AWS.Env
mkEnv region lg = do
  lgr <- newAwsLogger lg
  newEnv Discover
    <&> envLogger .~ lgr
    <&> envRegion .~ region
    <&> envRetryCheck .~ retryPolicy 5

newAwsLogger :: Monad m => (AWS.LogLevel -> LBS.ByteString -> IO ()) -> m AWS.Logger
newAwsLogger lg = return $ \y b ->
  let lazyMsg = toLazyByteString b
  in case L.toStrict lazyMsg of
      msg | BS.isInfixOf "404 Not Found" msg    -> lg AWS.Debug lazyMsg
      msg | BS.isInfixOf "304 Not Modified" msg -> lg AWS.Debug lazyMsg
      _                                         -> lg y lazyMsg

retryPolicy :: Int -> Int -> HttpException -> Bool
retryPolicy maxNum attempt ex = (attempt <= maxNum) && shouldRetry ex

shouldRetry :: HttpException -> Bool
shouldRetry ex = case ex of
  HttpExceptionRequest _ ctx -> case ctx of
    ResponseTimeout          -> True
    ConnectionTimeout        -> True
    ConnectionFailure _      -> True
    InvalidChunkHeaders      -> True
    ConnectionClosed         -> True
    InternalException _      -> True
    NoResponseDataReceived   -> True
    ResponseBodyTooShort _ _ -> True
    _                        -> False
  _ -> False
