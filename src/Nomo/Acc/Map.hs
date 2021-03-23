{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.Map (
    Map,
    Nomo.Acc.Map.map,
  ) where

import           GHC.TypeLits
import qualified Data.Map as Map

import           Nomo.Class

data Map    k v = Map    !(Map.Map k v)
data MapOdd k v = MapOdd !(Map.Map k v) k

map :: Map k v
map = Map Map.empty

instance
  (
    x ~ Map.Map k v
  )
  =>
  Stop (Map k v) x
  where
    stop (Map acc) = acc

instance
    TypeError (Text "The Nomo.map function must be applied to an even number of arguments.")
  =>
  Stop (MapOdd k v) x
  where
    stop = error "unreachable code"

instance
  (
    x ~ k
  )
  =>
  Step (Map k v) x (MapOdd k v)
  where
    step (Map acc) k = MapOdd acc k

instance
  (
    x ~ v
  ,
    Ord k
  )
  =>
  Step (MapOdd k v) x (Map k v)
  where
    step (MapOdd acc k) v = Map (Map.insert k v acc)
