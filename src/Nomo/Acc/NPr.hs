{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.NPr (
    NPr,
    npr,
  ) where

import           GHC.TypeLits
import           Data.Kind (Type)
import qualified Data.SOP.NP as NP

import           Nomo.Class

newtype NPr (f :: k -> Type) xs = NPr (NP.NP f xs)

npr :: NPr (f :: k -> Type) '[]
npr = NPr NP.Nil

instance
  (
    x ~ NP.NP f xs
  )
  =>
  Stop (NPr (f :: k -> Type) xs) x
  where
    stop (NPr acc) = acc

instance
  (
    x ~ f a
  ,
    xs' ~ (a ': xs)
  )
  =>
  Step (NPr (f :: k -> Type) xs) x (NPr f xs')
  where
    step (NPr acc) fa = NPr (fa NP.:* acc)
