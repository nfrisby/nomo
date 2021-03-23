{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.Bind (
    Bind,
    bind,
  ) where

import           Nomo.Class

newtype Bind m a = Bind (m a)

bind :: m a -> Bind m a
bind = Bind

instance
  (
    x ~ m a
  )
  =>
  Stop (Bind m a) x
  where
    stop (Bind m) = m

instance
  (
    x ~ (a -> m b)
  ,
    Monad m
  )
  =>
  Step (Bind m a) x (Bind m b)
  where
    step (Bind acc) f = Bind (acc >>= f)
