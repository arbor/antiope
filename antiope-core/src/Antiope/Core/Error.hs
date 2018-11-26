{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.Core.Error
  ( handle404ToNone
  ) where

import Control.Monad.Catch       (catch, throwM)
import Network.AWS               (Error (..), MonadAWS, ServiceError (..))
import Network.HTTP.Types.Status (Status (..))

import qualified Data.ByteString.Lazy as LBS

handle404ToNone :: MonadAWS m
  => m a
  -> m (Maybe a)
handle404ToNone f = do
  ebs <- (Right <$> f) `catch` \(err :: Error) -> case err of
    (ServiceError (ServiceError' _ (Status 404 _) _ _ _ _)) -> return (Left LBS.empty)
    _                                                       -> throwM err
  case ebs of
    Right bs -> return (Just bs)
    Left _   -> return Nothing
