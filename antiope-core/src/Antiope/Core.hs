{-# OPTIONS_GHC -fno-warn-orphans #-}

module Antiope.Core
  ( module Network.AWS.Data.Text
  , AWS.Env
  , AWS.HasEnv(..)
  , AWS.runAWS
  , AWS.send
  , AWS.runResourceT
  , AWS.liftAWS
  , AWS.sinkMD5, AWS.sinkSHA256
  , AWS.AWS
  , AWS.catching
  , AWS.Error(..)
  , AWS.ErrorCode(..), AWS.errorCode
  , AWS.Region(..)
  , AWS.LogLevel(..)
  ) where

import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Resource (ResourceT)
import Network.AWS.Data.Text        (FromText (..), Text, ToText (..), fromText, toText)

import qualified Network.AWS as AWS

instance AWS.MonadAWS m => AWS.MonadAWS (ResourceT m) where
  liftAWS = lift . AWS.liftAWS
