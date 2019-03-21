{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Antiope.SQS.Messages
  ( SqsMessage (..)
  , Message, mBody
  , QueueUrl
  , fromMessage, fromMessageMaybe
  , decodeBody
  ) where

import Antiope.SQS.Types
import Control.Lens      ((&), (<&>), (^.))
import Data.Aeson        as Aeson
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Network.AWS.SQS   (Message, mAttributes, mBody, mMD5OfBody, mMessageId, mReceiptHandle)

import qualified Data.HashMap.Strict as Hash
import qualified Data.Text.Encoding  as Text
import qualified Network.AWS.SQS     as SQS

-- | Converts 'Message` into 'SqsMessage' if its body is decodable as 'a'
fromMessageMaybe :: FromJSON a => Message -> Maybe (SqsMessage a)
fromMessageMaybe msg =
  let smsg = fromMessage msg
  in body smsg <&> (\a -> smsg { body = a})

-- | Converts a 'Message' into 'SqsMessage'
fromMessage :: FromJSON a => Message -> SqsMessage (Maybe a)
fromMessage msg =
  let attr a = msg ^. mAttributes & Hash.lookup a
  in SqsMessage
        { messageId                         = msg ^. mMessageId
        , receiptHandle                     = msg ^. mReceiptHandle <&> ReceiptHandle
        , md5OfBody                         = msg ^. mMD5OfBody
        , senderId                          = attr SQS.SenderId
        , sendTimestamp                     = attr SQS.SentTimestamp
        , approximateReceiveCount           = attr SQS.ApproximateReceiveCount
        , approximateFirstReceiveTimestamp  = attr SQS.ApproximateFirstReceiveTimestamp
        , body                              = decodeBody msg
        }

data SqsMessage a = SqsMessage
  { senderId                         :: !(Maybe Text)
  , messageId                        :: !(Maybe Text)
  , md5OfBody                        :: !(Maybe Text)
  , receiptHandle                    :: !(Maybe ReceiptHandle)
  , sendTimestamp                    :: !(Maybe Text)
  , approximateReceiveCount          :: !(Maybe Text)
  , approximateFirstReceiveTimestamp :: !(Maybe Text)
  , body                             :: !a
  } deriving (Show, Eq, Generic, Functor)

instance HasReceiptHandle (SqsMessage a) where
  getReceiptHandle = receiptHandle

-------------------------------------------------------------------------------

decodeBody :: FromJSON a => Message -> Maybe a
decodeBody msg =
  msg ^. mBody >>= (Aeson.decodeStrict . Text.encodeUtf8)
