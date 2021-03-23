{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.Splat (
    Splat,
    splat,
  ) where

import           Nomo.Class

-- splat:
-- ((pure assoc <*> pure show) <*> pure (*2)) <*> pure 3

newtype Splat f a = Splat (f a)

splat :: f a -> Splat f a
splat = Splat

instance
  (
    x ~ f a
  )
  =>
  Stop (Splat f a) x
  where
    stop (Splat fa) = fa

instance
  (
    x ~ f y 
  ,
    a ~ (y -> b)
  ,
    Applicative f
  )
  =>
  Step (Splat f a) x (Splat f b)
  where
    step (Splat acc) fy = Splat (acc <*> fy)
