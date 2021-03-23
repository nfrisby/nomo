{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.Compose (
    Compose,
    compose,
  ) where

import qualified Control.Category as Cat

import           Nomo.Class

newtype Compose cat a b = Compose (cat a b)

compose :: Cat.Category cat => Compose cat b b
compose = Compose Cat.id

instance
  (
    x ~ cat a b
  )
  =>
  Stop (Compose cat a b) x
  where
    stop (Compose acc) = acc

instance
  (
    x ~ cat a c
  ,
    Cat.Category cat
  )
  =>
  Step (Compose cat c b) x (Compose cat a b)
  where
    step (Compose arr) x = Compose (arr Cat.. x)
