{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.NP (
    NP,
    np,
  ) where

import           GHC.TypeLits
import           Data.Kind (Type)
import qualified Data.SOP.NP as NP

import           Nomo.Class

newtype NP (f :: k -> Type) ys xs = NP (NP.NP f ys -> NP.NP f xs)

np :: NP (f :: k -> Type) xs xs
np = NP id

instance
  (
    x ~ NP.NP f xs
  ,
    ys ~ '[]
  )
  =>
  Stop (NP (f :: k -> Type) ys xs) x
  where
    stop (NP acc) = acc NP.Nil

instance
  (
    x ~ f a
  ,
    ys ~ (a ': ys')
  )
  =>
  Step (NP (f :: k -> Type) ys xs) x (NP f ys' xs)
  where
    step (NP acc) fa = NP (acc . (fa NP.:*))
