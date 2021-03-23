{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.Vec (
    Vec,
    vec,
  ) where

import           GHC.TypeLits
import qualified Nomo.Data.Vec as V

import           Nomo.Class

newtype Vec m n a = Vec (V.Vec m a -> V.Vec n a)

vec :: Vec n n a
vec = Vec id

instance
  (
    x ~ V.Vec n a
  ,
    m ~ 0
  )
  =>
  Stop (Vec m n a) x
  where
    stop (Vec acc) = acc V.Nil

instance
  (
    x ~ a
  ,
    m' ~ (m - 1)    -- ^ needed for fundep, without a Nat solver
  )
  =>
  Step (Vec m n a) x (Vec m' n a)
  where
    step (Vec acc) a = Vec (acc . V.Cons a)
