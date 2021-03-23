{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.Pre (
    Pre,
    pre,
  ) where

import           Nomo.Class

data Pre a b acc = Pre (a -> b) !acc

infixr `pre`

pre :: (a -> b) -> acc -> Pre a b acc
pre = Pre

instance
    Stop acc x
  =>
  Stop (Pre a b acc) x
  where
    stop (Pre _f acc) = stop acc

instance
  (
    x ~ a
  ,
    Step acc b acc'
  )
  =>
  Step (Pre a b acc) x (Pre a b acc')
  where
    step (Pre f acc) a = Pre f (step acc (f a))
