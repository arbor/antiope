module Antiope.Core
( module Network.AWS.Data.Text
, AWS.Env(..)
, AWS.HasEnv(..)
, AWS.MonadAWS
, AWS.runAWS
, AWS.runResourceT
, AWS.Region(..)
, AWS.LogLevel(..)
)
where

import           Network.AWS.Data.Text (FromText (..), Text, ToText (..),
                                        fromText, toText)

import qualified Network.AWS           as AWS

