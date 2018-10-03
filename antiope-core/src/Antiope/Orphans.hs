{-# OPTIONS_GHC -fno-warn-orphans #-}

module Antiope.Orphans where

import Control.Monad.Trans          (lift)
import Control.Monad.Trans.Resource (ResourceT)
import Network.AWS                  (MonadAWS (..))

instance MonadAWS m => MonadAWS (ResourceT m) where
  liftAWS = lift . liftAWS
