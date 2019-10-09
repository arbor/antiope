{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Antiope.ES.Sign where

import Control.Lens              (Lens', lens, (&), (.~), (<&>))
import Data.Aeson
import Data.JsonStream.Parser    (objectWithKey, parseByteString)
import Data.Vector               (Vector)
import Network.AWS.ElasticSearch (elasticSearch)
import Network.AWS.Prelude
import Network.AWS.Request       (postBody)
import Network.AWS.Response      (receiveBytes)
import Network.AWS.Sign.V4       (v4)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict        as HMap
import qualified Data.JsonStream.Parser     as JP
import qualified Data.Vector                as Vector

esService :: Service
esService = Service
  { _svcAbbrev    = "ElasticSearchClient"
  , _svcSigner    = v4
  , _svcPrefix    = "es"
  , _svcVersion   = _svcVersion elasticSearch
  , _svcEndpoint  = defaultEndpoint esService <&> endpointHost .~ error "ElasticSearch Service endpoint is not configured. example: 'let env2 = configure (setEndpoint True <hostname> 443 esService) env'"
  , _svcTimeout   = _svcTimeout elasticSearch
  , _svcCheck     = _svcCheck elasticSearch
  , _svcError     = _svcError elasticSearch
  , _svcRetry     = _svcRetry elasticSearch
  }

data SendBulk a = SendBulk
  { _pbOps           :: Vector a
    -- This is here as opposed to in a class in order to avoid forcing orphan
    -- instances when we integrate with Bloodhound.
  , _encodeEsRequest :: Vector a -> L.ByteString
  } deriving (Typeable, Generic)

sendBulk :: Vector a -> (Vector a -> L.ByteString) -> SendBulk a
sendBulk = SendBulk

sendBulk' :: [a] -> (Vector a -> L.ByteString) -> SendBulk a
sendBulk' = SendBulk . Vector.fromList

bulkOps :: Lens' (SendBulk a) (Vector a)
bulkOps = lens _pbOps (\s a -> s{_pbOps = a})

bulkEncode :: Lens' (SendBulk a) (Vector a -> L.ByteString)
bulkEncode = lens _encodeEsRequest (\s a -> s{_encodeEsRequest = a})

instance ToPath (SendBulk a) where
  toPath = const "_bulk"

instance ToQuery (SendBulk a) where
  toQuery = const mempty

instance ToHeaders (SendBulk a) where
  toHeaders = const [(hContentType, "application/json")]

instance AWSRequest (SendBulk a) where
  type Rs (SendBulk a) = SendBulkResponse
  request = postBody esService
  response = receiveBytes $ \_ _ x ->
    -- ES returns JSON which contains an array of elements, one per an item in a batch.
    -- Parsing this massive response is expensive (it takes ~15%-20% of the runtime)
    -- and it is only needed in case of errors.
    -- As an optimisation, we can first look at the "errors" field, and if it is set
    -- to False we can avoid parsing the rest of the response
    let
      -- We could use '(,) <$> objectWithKey "errors" <*> objectWithKey "took"'
      -- and it works, except that somehow it forces JSON Parser to traverse
      -- the whole response, which we want to avoid.
      -- Looks like it is a property of the Applicative instance for the Parser.
      errors = parseByteString (objectWithKey "errors" JP.bool) x & listToMaybe
      took'  = parseByteString (objectWithKey "took" JP.number) x & listToMaybe
    in case (,) <$> took' <*> errors of
      Just (time, False) -> Right $ SendBulkResponse (round time) False []
      _                  -> JP.eitherDecodeStrict x

instance ToBody (SendBulk a) where
  toBody a = toBody $ _encodeEsRequest a $ _pbOps a

data SendBulkResponse = SendBulkResponse
  { took       :: !Int
  , hasErrors  :: !Bool
  , operations :: ![SendBulkResponseOperation]
  } deriving (Eq, Show, Typeable, Generic)

data SendBulkResponseOperation = SendBulkResponseOperation
  { action :: !Text
  , item   :: !SendBulkResponseItem
  } deriving (Eq, Show, Typeable, Generic)

data SendBulkResponseItem = SendBulkResponseItem
  { index     :: !Text
  , mapping   :: !Text
  , itemId    :: !Text
  , status    :: !Int
  , errorJson :: !(Maybe Value)
  } deriving (Eq, Show, Typeable, Generic)

instance FromJSON SendBulkResponseItem where
  parseJSON =
    withObject "SendBulkResponseItem" $ \obj -> SendBulkResponseItem
      <$> obj .: "_index"
      <*> obj .: "_type"
      <*> obj .: "_id"
      <*> obj .: "status"
      <*> obj .:? "error"

instance ToJSON SendBulkResponseItem where
  toJSON a = object
    [ "_index" .= index a
    , "_type"  .= mapping a
    , "_id"    .= itemId a
    , "status" .= status a
    , "error"  .= errorJson a
    ]

instance FromJSON SendBulkResponseOperation where
  parseJSON =
    withObject "SendBulkResponseOperation" $ \obj ->
      case HMap.keys obj of
        [actionName] -> SendBulkResponseOperation actionName <$> obj .: actionName
        _            -> fail "Unable to parse bulk response item action"

instance ToJSON SendBulkResponseOperation where
  toJSON a = object [ action a .= item a]

instance FromJSON SendBulkResponse where
  parseJSON =
    withObject "SendBulkResponse" $ \obj -> SendBulkResponse
      <$> obj .: "took"
      <*> obj .: "errors"
      <*> obj .: "items"

instance ToJSON SendBulkResponse where
  toJSON a = object
    [ "took"    .= took a
    , "errors"  .= hasErrors a
    , "items"   .= operations a
    ]
