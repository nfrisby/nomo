{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.RApp (
    RApp,
    rapp,
  ) where

import           Nomo.Control.Apply (Apply (..))

import           Nomo.Class

newtype RApp a = RApp a

rapp :: a -> RApp a
rapp = RApp

instance
  (
    a ~ x
  )
  =>
  Stop (RApp a) x
  where
    stop (RApp a) = a

instance
    Apply x a b
  =>
  Step (RApp a) x (RApp b)
  where
    step (RApp a) x = RApp $ apply x a
