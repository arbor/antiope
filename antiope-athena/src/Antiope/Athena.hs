module Antiope.Athena
  ( query
  , queryExecutionSucceed
  , module Network.AWS.Athena
  ) where

import Control.Lens
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text               (Text)
import Network.AWS             (HasEnv, Rs)
import Network.AWS.Athena
import Network.AWS.Waiter      hiding (accept)

import qualified Network.AWS as AWS

query :: (HasEnv e, MonadUnliftIO m)
  => e
  -> ResultConfiguration
  -> QueryExecutionContext
  -> Text
  -> Text
  -> m [Row]
query e config context qstring clientRequestToken = AWS.runResourceT $ AWS.runAWS e $ do
  resp <- AWS.send $ startQueryExecution qstring config
    & sqeQueryExecutionContext  ?~ context
    & sqeClientRequestToken     ?~ clientRequestToken
  case resp ^. sqersQueryExecutionId of
    Just qeid -> go qeid
    Nothing   -> return []
  where go qeid = do
          accept <- AWS.await queryExecutionFinished (getQueryExecution qeid)
          case accept of
            AcceptSuccess -> do
              results <- AWS.send $ getQueryResults qeid
              case view gqrrsResultSet results of
                Just rs -> return $ view rsRows rs
                Nothing -> return []
            AcceptRetry   -> go qeid
            _             -> return []

queryExecutionFinished :: Wait GetQueryExecution
queryExecutionFinished = Wait
    { _waitName = "QueryFinished"
    , _waitAttempts = 100
    , _waitDelay = 1
    , _waitAcceptors =
      [ matchAll Succeeded AcceptSuccess status
      , matchAll Failed    AcceptFailure status
      , matchAll Cancelled AcceptFailure status
      , matchAll Queued    AcceptRetry   status
      , matchAll Running   AcceptRetry   status
      ]
    }
    where status :: Fold (Rs GetQueryExecution) QueryExecutionState
          status = gqersQueryExecution . _Just . qeStatus . _Just . qesState . _Just

queryExecutionSucceed :: (MonadUnliftIO m, HasEnv e)
  => e
  -> Text
  -> m Bool
queryExecutionSucceed e qeid = AWS.runResourceT $ AWS.runAWS e $ do
  eq <- view gqersQueryExecution <$> AWS.send (getQueryExecution qeid)
  case view qesState =<< (view qeStatus =<< eq) of
    Just Succeeded -> return True
    Just _         -> return False
    Nothing        -> return False
