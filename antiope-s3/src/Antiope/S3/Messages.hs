{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
module Antiope.S3.Messages
  ( EventName(..)
  , S3Message(..)
  , messageToS3Uri
  , fromSnsRecords
  ) where

import Antiope.Messages
import Antiope.S3            (BucketName (..), ETag (..), ObjectKey (..))
import Control.Lens          (coerced, each, (^.))
import Data.Aeson            as Aeson
import Data.Int              (Int64)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import GHC.Generics          (Generic)
import Network.AWS.Data.Text (toText)

import qualified Antiope.S3.Types   as Types
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Network.URI        as URI


messageToS3Uri :: S3Message -> Types.S3Uri
messageToS3Uri msg = Types.S3Uri (bucket msg) (key msg)

fromSnsRecords :: Text -> [S3Message]
fromSnsRecords msg =
  Aeson.decodeStrict' @(WithEncoded "Message" (With "Records" [S3Message])) (T.encodeUtf8 msg)
    ^. each . coerced

data EventName = EventName
  { eventType :: !Text
  , action    :: !Text
  } deriving (Show, Eq, Ord, Generic)

data S3Message = S3Message
  { eventTime :: !(Maybe UTCTime)
  , eventName :: !EventName
  , bucket    :: !BucketName
  , key       :: !ObjectKey
  , size      :: !Int64
  , eTag      :: !(Maybe ETag)
  } deriving (Show, Eq, Generic)

instance FromJSON S3Message where
  parseJSON = withObject "S3Message" $ \obj -> do
    eTime <- obj .:? "eventTime"
    eName <- readEventName <$> obj .: "eventName"
    s3    <- obj .: "s3"
    bkt   <- s3 .: "bucket"
    obj'  <- s3 .: "object"
    bName <- BucketName <$> (bkt .: "name")
    oKey  <- (ObjectKey . T.pack . URI.unEscapeString . T.unpack) <$> (obj' .: "key")
    oSize <- obj' .: "size"
    oEtag  <- fmap (ETag . T.encodeUtf8) <$> (obj' .:? "eTag")
    pure $ S3Message eTime eName bName oKey oSize oEtag

instance ToJSON S3Message where
  toJSON msg = Aeson.object
    [ "eventTime" .= eventTime msg
    , "eventName" .= T.intercalate ":" [eventType (eventName msg), action (eventName msg)]
    , "s3"        .= Aeson.object
                      [ "bucket"    .= object [ "name" .= toText (bucket msg)]
                      , "object"    .= object
                                        [ "key"  .= (T.pack . URI.escapeURIString URI.isAllowedInURI . T.unpack . toText . key) msg
                                        , "size"      .= size msg
                                        , "eTag"      .= fmap toText (eTag msg)
                                        ]
                      ]
    ]

readEventName :: T.Text -> EventName
readEventName evt =
  case T.splitOn ":" evt of
    [t, e]       -> EventName t e
    ["s3", t, e] -> EventName t e
    (t:e:_)      -> EventName t e
    _            -> EventName evt ""

