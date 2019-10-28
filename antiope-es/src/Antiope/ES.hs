module Antiope.ES
( X.esService
, X.RawRequest(..)
, X.jsonRequest
, X.RawResponse(..)
, QueryString
, Header
)
where

import qualified Antiope.ES.Service as X

import Network.AWS.Data.Headers (Header)
import Network.AWS.Data.Query   (QueryString)
