module Antiope.Env
( mkEnv
, AWS.Env
, AWS.LogLevel
, AWS.Region
)
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.AWS       hiding (LogLevel)
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as LBS

import qualified Network.AWS                   as AWS
import           Network.HTTP.Client           (HttpException (..),
                                                HttpExceptionContent (..))

mkEnv :: Region -> (AWS.LogLevel -> LBS.ByteString -> IO ()) -> IO AWS.Env
mkEnv region log = do
  lgr <- newAwsLogger log
  newEnv Discover
    <&> envLogger .~ lgr
    <&> envRegion .~ region
    <&> envRetryCheck .~ retryPolicy 5

newAwsLogger :: Monad m => (AWS.LogLevel -> LBS.ByteString -> IO ()) -> m AWS.Logger
newAwsLogger log = return $ \y b ->
  case toLazyByteString b of
    msg | BS.isInfixOf (C8.pack "404 Not Found") (L.toStrict msg) -> pure ()
    msg -> log y msg

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
