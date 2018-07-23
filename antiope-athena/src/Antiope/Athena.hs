module Antiope.Athena
  ( query
  , queryExecutionSucceed
  , module Network.AWS.Athena
  )where

import Control.Lens
import Control.Monad.Trans.AWS      hiding (await, send)
import Control.Monad.Trans.Resource
import Data.Text                    (Text)
import Network.AWS                  (MonadAWS, await, send)
import Network.AWS.Athena
import Network.AWS.Waiter           hiding (accept)

query :: (MonadResource m, MonadAWS m)
            => ResultConfiguration
            -> QueryExecutionContext
            -> Text
            -> Text
            -> m [Row]
query config context qstring clientRequestToken = do
  resp <- send $ startQueryExecution qstring config
                    & sqeQueryExecutionContext ?~ context
                    & sqeClientRequestToken ?~ clientRequestToken
  case resp ^. sqersQueryExecutionId of
    Just qeid -> go qeid
    Nothing   -> return []
  where
    go qeid = do
      accept <- await queryExecutionFinished (getQueryExecution qeid)
      case accept of
        AcceptSuccess -> do
          results <- send $ getQueryResults qeid
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
    where
      status :: Fold (Rs GetQueryExecution) QueryExecutionState
      status = gqersQueryExecution . _Just . qeStatus . _Just . qesState . _Just

queryExecutionSucceed :: (MonadResource m, MonadAWS m)
                        => Text
                        -> m Bool
queryExecutionSucceed qeid = do
  eq <- view gqersQueryExecution <$> send (getQueryExecution qeid)
  case view qesState =<< (view qeStatus =<< eq) of
    Just Succeeded -> return True
    Just _         -> return False
    Nothing        -> return False
