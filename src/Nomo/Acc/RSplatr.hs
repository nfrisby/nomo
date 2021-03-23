{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.RSplatr (
    RSplatr,
    rsplatr,
  ) where

import           Nomo.Class

-- nomo $ Nomo.rsplatr (pure 3) (pure (*2)) (pure show)

newtype RSplatr f a = RSplatr (f a)

rsplatr :: f a -> RSplatr f a
rsplatr = RSplatr

instance
  (
    x ~ f a
  )
  =>
  Stop (RSplatr f a) x
  where
    stop (RSplatr fa) = fa

instance
  (
    x ~ f (a -> b)
  ,
    Applicative f
  )
  =>
  Step (RSplatr f a) x (RSplatr f b)
  where
    step (RSplatr acc) ff = RSplatr (ff <*> acc)
