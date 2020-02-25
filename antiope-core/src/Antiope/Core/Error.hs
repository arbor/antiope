{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Antiope.Core.Error
  ( handle404ToNone
  , handleServiceError
  ) where

import Control.Monad.Catch       (catch, throwM)
import Network.AWS               (Error (..), MonadAWS, ServiceError (..))
import Network.HTTP.Types.Status (Status (..))

handle404ToNone :: MonadAWS m
  => m a
  -> m (Maybe a)
handle404ToNone f =
  handleServiceError f Just $ \case
    (Status 404 _) -> Just Nothing
    _              -> Nothing

handleServiceError :: MonadAWS m
  => m a                  -- ^ AWS action
  -> (a -> b)             -- ^ Transform successful result
  -> (Status -> Maybe b)  -- ^ Compensate with the failure. 'Nothing' means that the error is not handled
  -> m b                  -- ^ Final result. In case of unhandled errors the exception is re-thrown.
handleServiceError ma success failure =
  (success <$> ma) `catch` \(err :: Error) -> case err of
    (ServiceError (ServiceError' _ status _ _ _ _)) -> maybe (throwM err) pure (failure status)
    _                                               -> throwM err
