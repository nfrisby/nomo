{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Nomo.Acc.Foldr (
    Foldr,
    Nomo.Acc.Foldr.foldr,
  ) where

import           Nomo.Class

data Foldr a b = Foldr (a -> b -> b) b (b -> b)

foldr :: (a -> b -> b) -> b -> Foldr a b
foldr f z = Foldr f z id

instance
  (
    x ~ b
  )
  =>
  Stop (Foldr a b) x
  where
    stop (Foldr _f z acc) = acc z

instance
  (
    x ~ a
  )
  =>
  Step (Foldr a b) x (Foldr a b)
  where
    step (Foldr f z acc) a = Foldr f z (acc . f a)
