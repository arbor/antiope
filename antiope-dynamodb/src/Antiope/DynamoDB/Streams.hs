module Antiope.DynamoDB.Streams
(
-- ** Consuming DynamoDB streams
  Limit(..)
, StreamArn(..)
, StreamIterator
, runIterator
, readIterator, tryReadIterator

-- ** Lower level primitives
, getShards
, createShardIterator
, fetchShardRecords

-- ** Async handlers
, runStreamAsync
, runShardAsync
)
where

import Antiope.DynamoDB.Types
import Control.Concurrent.Chan.Unagi.Bounded (newChan, readChan, tryRead, tryReadChan, writeList2Chan)
import Control.Lens
import Control.Monad                         (forM_, void)
import Control.Monad.IO.Class                (liftIO)
import Control.Monad.Trans.Resource          (MonadResource, register)
import Data.Coerce                           (coerce)
import Data.Maybe                            (catMaybes)
import Network.AWS                           (MonadAWS)
import Network.AWS.DynamoDBStreams
import UnliftIO                              (MonadUnliftIO)
import UnliftIO.Async                        (forConcurrently)
import UnliftIO.Concurrent                   (forkIO, killThread)

import qualified Network.AWS as AWS

-- | Creates and runs a stream iterator.
-- Values can be polled from the iterator using 'readIterator' and 'tryReadIterator'  functions.
--
-- A 'StreamIterator' is something that runs in background (using 'forkIO')
-- and polls messages from the specified DynamoDB stream.
-- The background process is stopped when a 'runResourceT' happens
-- for a given resource context.
runIterator :: (MonadAWS m, MonadUnliftIO m, MonadResource m)
  => Limit
  -> StreamArn
  -> m StreamIterator
runIterator (Limit limit) arn = do
  (inC, outC) <- liftIO $ newChan (fromIntegral limit)
  runStreamAsync (Limit limit) arn (liftIO . writeList2Chan inC)
  pure $ StreamIterator outC

-- | Reads from the iterator, blocking if it contains no messages.
readIterator :: MonadUnliftIO m => StreamIterator -> m Record
readIterator (StreamIterator outC) = liftIO $ readChan outC

-- | Returns immediately whether the iterator has a message or not.
tryReadIterator :: MonadUnliftIO m => StreamIterator -> m (Maybe Record)
tryReadIterator (StreamIterator outC) = liftIO $ tryReadChan outC >>= (tryRead . fst)

-- | Lists shards for a given stream
getShards :: MonadAWS m => StreamArn -> m [ShardId]
getShards (StreamArn arn) =
  AWS.send (describeStream arn) <&> toListOf (dsrsStreamDescription . each . sdShards . each . sShardId . each . to ShardId)

-- | Creates an iterator for a given shard.
createShardIterator :: MonadAWS m => StreamArn -> ShardId -> m (Maybe ShardIterator)
createShardIterator (StreamArn arn) (ShardId sid) =
  AWS.send (getShardIterator arn sid Latest) <&> view gsirsShardIterator <&> coerce

-- | Returns a batch of records of size @limit@ from a given shard iterator,
-- alongside with a new shard iterator to continue reading the stream with.
--
-- A new iterator can be 'Nothing' when the shard is closed and will not
-- contain new messages.
fetchShardRecords :: MonadAWS m => Limit -> ShardIterator -> m (Maybe ShardIterator, [Record])
fetchShardRecords (Limit limit) (ShardIterator it) = do
  resp <- AWS.send (getRecords it & grLimit ?~ fromIntegral limit)
  let nextIter = resp ^. grrsNextShardIterator & coerce
  let keys = resp ^. grrsRecords
  pure (nextIter, keys)

-- Handles records from the stream asynchronously with a handler function provided.
--
-- Runs several background workers, which are stopped when 'runResourceT' happens for
-- a given resource context.
runStreamAsync :: (MonadAWS m, MonadUnliftIO m, MonadResource m) => Limit -> StreamArn -> ([Record] -> m ()) -> m ()
runStreamAsync limit arn handler = do
  sids   <- getShards arn
  shards <- forConcurrently sids (createShardIterator arn) <&> catMaybes
  forM_ shards (\s -> runShardAsync limit s handler)

-- Handles records from the shard asynchronously with a handler function provided.
--
-- Runs several background workers, which are stopped when 'runResourceT' happens for
-- a given resource context.
runShardAsync :: (MonadAWS m, MonadUnliftIO m, MonadResource m) => Limit -> ShardIterator -> ([Record] -> m ()) -> m ()
runShardAsync limit shardIter handle =
  forkIO (go shardIter) >>= (void . register . killThread)
  where
    go iter = do
      (mbIter, batch) <- fetchShardRecords limit iter
      handle batch
      case mbIter of
        Nothing    -> pure ()
        Just iter' -> go iter'
