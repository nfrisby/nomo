{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.RSplat (
    RSplat,
    rsplat,
  ) where

import           Nomo.Class

-- splat:
-- ((pure assoc <*> pure show) <*> pure (*2)) <*> pure 3

-- rsplat:
-- pure 3 <+> (pure (*2) <+> (pure show <+> pure assoc))

-- infixr <+>
-- (<+>) :: Applicative f => f a -> f (a -> b) -> f b
-- (<+>) = flip (<*>)

data RSplat f b a r = RSplat (f b -> f r) (f a)

rsplat :: Applicative f => f a -> RSplat f r a r
rsplat = RSplat id

instance
  (
    x ~ f r
  ,
    a ~ b
  )
  =>
  Stop (RSplat f b a r) x
  where
    stop (RSplat acc a) = acc a

instance
  (
    x ~ f a2
  ,
    b2 ~ (a1 -> b1)
  ,
    Applicative f
  )
  =>
  Step (RSplat f b1 a1 r) x (RSplat f b2 a2 r)
  where
    step = step_

step_ ::
    Applicative f
  =>
  RSplat f b1 a1 r ->
  f a2 ->
  RSplat f (a1 -> b1) a2 r
step_ (RSplat acc fa1) fa2 =
    RSplat
      (\next -> acc $ next <*> fa1)
      fa2
