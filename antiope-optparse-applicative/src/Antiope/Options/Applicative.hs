module Antiope.Options.Applicative
  ( autoText
  ) where

import Network.AWS.Data.Text
import Options.Applicative

import qualified Data.Text as T

autoText :: FromText a => ReadM a
autoText = eitherReader $ fromText . T.pack
