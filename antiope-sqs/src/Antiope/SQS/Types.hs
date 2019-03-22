{-# LANGUAGE DeriveGeneric #-}
module Antiope.SQS.Types
where

import Control.Lens          ((<&>), (^.))
import Data.String           (IsString)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Network.AWS.Data.Text (FromText (..), ToText (..))
import Network.AWS.SQS       (Message, mReceiptHandle)

-- | Queue consuming mode
data ConsumerMode
  = Drain   -- ^ Keep consuming the queue until there are no messages
  | Forever -- ^ Keep consuming the queue forever
  deriving (Show, Eq, Generic)

data ConsumerResult
  = Ack
  | Nack
  deriving (Show, Eq, Generic)

data SQSError = DeleteMessageBatchError
  deriving (Eq, Show, Generic)

newtype QueueUrl = QueueUrl Text
  deriving (Show, Eq, IsString, FromText, ToText, Generic)

newtype ReceiptHandle = ReceiptHandle Text
  deriving (Show, Read, Eq, Ord, FromText, ToText)

class HasReceiptHandle a where
  getReceiptHandle :: a -> Maybe ReceiptHandle

instance HasReceiptHandle Message where
  getReceiptHandle msg = msg ^. mReceiptHandle <&> ReceiptHandle

instance HasReceiptHandle ReceiptHandle where
  getReceiptHandle = Just . id
