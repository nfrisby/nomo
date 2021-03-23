{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Nomo.Acc.Foldl (
    Foldl,
    Nomo.Acc.Foldl.foldl,
  ) where

import           Nomo.Class

data Foldl a b = Foldl (b -> a -> b) !b

foldl :: (b -> a -> b) -> b -> Foldl a b
foldl = Foldl

instance
  (
    x ~ b
  )
  =>
  Stop (Foldl a b) x
  where
    stop (Foldl _f acc) = acc

instance
  (
    x ~ a
  )
  =>
  Step (Foldl a b) x (Foldl a b)
  where
    step (Foldl f acc) a = Foldl f (f acc a)
