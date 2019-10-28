{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeFamilies      #-}

{-| Elasticsearch client service for managed AWS ElasticSearch cluser.

    Going to the ES through Amazonka/AWS is sometimes important because
    unlike "plain" ES, managed ES often requires each HTTP(S) request to be signed.

    Amazonka handles signing bits automatically therefore accessing managed ES
    as a normal Amazonka service works as expected.

    This client only contains low-level(ish) API that is expressed in terms of the
    'RawRequest' and 'RawResponse' types.

    Concrete operations, like Insert, Update or Bulk can be implemented in terms of
    'RawRequest' and 'RawResponse'.
-}
module Antiope.ES.Service where

import Control.Lens              ((&), (.~), (<&>))
import Data.JsonStream.Parser    (objectWithKey, parseByteString)
import Network.AWS.Data.Headers  (Header)
import Network.AWS.Data.Query    (QueryString)
import Network.AWS.ElasticSearch (elasticSearch)
import Network.AWS.Prelude
import Network.AWS.Request       (postBody)
import Network.AWS.Response      (receiveBytes)
import Network.AWS.Sign.V4       (v4)

import qualified Data.JsonStream.Parser as JP

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

-- | Raw request type. Every concrete ES operation can be reduced to this simple form.
data RawRequest = RawRequest
  { requestPath        :: !ByteString       -- ^ ES path for a request, like "_bulk" or "_search"
  , requestQueryString :: !QueryString      -- ^ Query string parameters (use 'mempty' when none required)
  , requestHeaders     :: ![Header]         -- ^ A list of headers. Typically ContentType: '[(hContentType, "application/json")]'
  , requestBody        :: !LazyByteString   -- ^ Raw body. This client will not check the correctness of its content.
  } deriving (Eq, Show, Typeable, Generic)

-- | Raw response type. Only minimal amount of information is parsed to get the result
--   of this type, further deserialisation is up to a concrete ES operation handler.
data RawResponse = RawResponse
  { responseTook     :: !Int                -- ^ How much time did it take to execute the request
  , resposeHasErrors :: !Bool               -- ^ There was an error in processing the request
  , responseBody     :: !ByteString         -- ^ The resonse body as is.
  } deriving (Eq, Show, Typeable, Generic)

-- | Creates JSON response with 'ContentType: application/json'
jsonRequest :: ByteString -> QueryString -> LazyByteString -> RawRequest
jsonRequest path query = RawRequest path query [(hContentType, "application/json")]


instance ToPath RawRequest where toPath = requestPath
instance ToQuery RawRequest where toQuery = requestQueryString
instance ToHeaders RawRequest where toHeaders = requestHeaders
instance ToBody RawRequest where toBody = toBody . requestBody

instance AWSRequest RawRequest where
  type Rs RawRequest = RawResponse
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
      Just (time, hasErr) -> Right $ RawResponse (round time) hasErr x
      Nothing             -> Left $ "Unable to parse ES Response: " <> show x
