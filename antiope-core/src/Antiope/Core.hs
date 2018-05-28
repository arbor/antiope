module Antiope.Core
( module Network.AWS.Data.Text
, AWS.Env(..)
, AWS.HasEnv(..)
, AWS.MonadAWS
, AWS.runAWS
, AWS.send
, AWS.runResourceT
, AWS.liftAWS
, AWS.sinkMD5, AWS.sinkSHA256
, AWS.AWS(..)
, AWS.catching
, AWS.Error(..)
, AWS.ErrorCode(..), AWS.errorCode
, AWS.Region(..)
, AWS.LogLevel(..)
)
where

import           Network.AWS.Data.Text (FromText (..), Text, ToText (..),
                                        fromText, toText)

import qualified Network.AWS           as AWS

