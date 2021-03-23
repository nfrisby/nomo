{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Nomo.Acc.Foldr1 (
    Foldr1,
    Nomo.Acc.Foldr1.foldr1,
  ) where

import           Nomo.Class

data Foldr1 a b = Foldr1 (a -> b -> b) (a -> b) a (b -> b)

foldr1 :: (a -> b -> b) -> (a -> b) -> a -> Foldr1 a b
foldr1 f zf latest = Foldr1 f zf latest id

instance
  (
    x ~ b
  )
  =>
  Stop (Foldr1 a b) x
  where
    stop (Foldr1 _f zf latest acc) = acc (zf latest)

instance
  (
    x ~ a
  )
  =>
  Step (Foldr1 a b) x (Foldr1 a b)
  where
    step (Foldr1 f zf latest acc) a = Foldr1 f zf a (acc . f latest)
