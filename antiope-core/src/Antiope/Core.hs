{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Antiope.Core
  ( module Network.AWS.Data.Text
  , AWS.AWS
  , AWS.MonadAWS(..)
  , AWS.Env
  , AWS.Error(..)
  , AWS.ErrorCode(..), AWS.errorCode
  , AWS.HasEnv(..)
  , AWS.LogLevel(..)
  , AWS.Region(..)
  , AWS.catching
  , AWS.runAWS
  , AWS.runResourceT
  , AWS.send
  , AWS.sinkMD5, AWS.sinkSHA256
  , MonadResource
  , ToLogStr(..)
  , runAws
  , runAwsThe
  , runAwsTyped
  , runResAws
  , runResAwsThe
  , runResAwsTyped
  ) where

import Control.Lens                 (view)
import Control.Monad.Logger         (ToLogStr (..))
import Control.Monad.Reader         (MonadReader)
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, ResourceT)
import Data.Generics.Product.Any
import Data.Generics.Product.Typed
import Network.AWS.Data.Text        (FromText (..), Text, ToText (..), fromText, toText)

import qualified Control.Monad.Trans.AWS as AWS hiding (send)
import qualified Network.AWS             as AWS

instance AWS.MonadAWS m => AWS.MonadAWS (ResourceT m) where
  liftAWS = lift . AWS.liftAWS

runAws :: (MonadResource m, AWS.HasEnv r) => r -> AWS.AWS a -> m a
runAws = AWS.runAWS

runResAws :: (MonadUnliftIO m, AWS.HasEnv r) => r -> AWS.AWS a -> m a
runResAws r = AWS.runResourceT . AWS.runAWS r

runAwsThe :: forall m r e s a.
  ( MonadUnliftIO m
  , MonadReader r m
  , HasAny s r r e e
  , HasEnv e)
  => AWS.AWS a
  -> m a
runAwsThe f = do
  e <- view $ the @s
  AWS.runResourceT $ AWS.runAWS e f

runResAwsThe :: forall m r e s a.
  ( MonadResource m
  , MonadReader r m
  , HasAny s r r e e
  , HasEnv e)
  => AWS.AWS a
  -> m a
runResAwsThe f = do
  e <- view $ the @s
  AWS.runAWS e f

runAwsTyped :: forall m r a.
  ( MonadUnliftIO m
  , MonadReader r m
  , HasType AWS.Env r)
  => AWS.AWS a
  -> m a
runAwsTyped f = do
  e <- view $ typed @AWS.Env
  AWS.runResourceT $ AWS.runAWS e f

runResAwsTyped :: forall m r a.
  ( MonadResource m
  , MonadReader r m
  , HasType AWS.Env r)
  => AWS.AWS a
  -> m a
runResAwsTyped f = do
  e <- view $ typed @AWS.Env
  AWS.runAWS e f
