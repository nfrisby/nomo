{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.PrePoly (
    PrePoly,
    prePoly,
  ) where

import           Nomo.Control.Apply

import           Nomo.Class

data PrePoly fun acc = PrePoly fun !acc

infixl `prePoly`

prePoly :: fun -> acc -> PrePoly fun acc
prePoly = PrePoly

instance
    Stop acc x
  =>
  Stop (PrePoly fun acc) x
  where
    stop (PrePoly _f acc) = stop acc

instance
  (
    Apply fun x y
  ,
    Step acc y acc'
  )
  =>
  Step (PrePoly fun acc) x (PrePoly fun acc')
  where
    step (PrePoly f acc) a = PrePoly f (step acc (apply f a))
