{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Antiope.Messages.Via where

import qualified Antiope.Messages.Types as T
import qualified Data.Kind              as K
import qualified GHC.TypeLits           as TL

type (/) (s :: TL.Symbol) (k :: K.Type) = T.With s k
infixr 2 /

type (//) (s :: TL.Symbol) (k :: K.Type) = T.WithEncoded s k
infixr 2 //
