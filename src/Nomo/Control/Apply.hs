{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Control.Apply (
    Apply (..),
    ArrowF (..),
    Compose (..),
    Dual (..),
    Id (..),
    SopI (..),
    ToArrowA (..),
    ToArrowM (..),
    ToDual (..),
    ToKleisli (..),
  ) where

import qualified Control.Arrow as Arrow
import qualified Control.Category as Cat
import           Data.Kind (Type)
import qualified Data.SOP as SOP

-----

-- | This /belongs/ in ~~a museum~~ the @base@ libraries!
class
  Apply fun dom cod
  | fun dom -> cod
  where
  apply :: fun -> dom -> cod

-----

instance (x ~ a) => Apply (a -> b) x b where apply = id

-----

instance
  (
    dom ~ a
  )
  =>
  Apply (Arrow.Kleisli m a b) dom (m b)
  where
    apply = Arrow.runKleisli

-----

data ToKleisli (m :: Type -> Type) = ToKleisli

instance
  (
    dom ~ (a -> m b)
  )
  =>
  Apply (ToKleisli m) dom (Arrow.Kleisli m a b)
  where
    apply _ = Arrow.Kleisli

-----

data Id = Id

instance
  (
    cod ~ dom
  )
  =>
  Apply Id dom cod
  where
    apply _ = id

-----

data SopI = SopI

instance
  (
    cod ~ SOP.I dom
  )
  =>
  Apply SopI dom cod
  where
    apply _ = SOP.I

-----

data ToArrowM (m :: Type -> Type) = ToArrowM

-- | The effects of the rightmost 'ArrowF' happen first
instance
  (
    dom ~ (a -> m b)
  ,
    Monad m
  )
  =>
  Apply (ToArrowM m) dom (ArrowF m a b)
  where
    apply _ f = ArrowF (>>= f)

-----

newtype ArrowF (m :: Type -> Type) a b = ArrowF (m a -> m b)

instance Cat.Category (ArrowF m) where
  id = ArrowF id
  ArrowF f . ArrowF g = ArrowF (f . g)

instance (x ~ m a) => Apply (ArrowF m a b) x (m b) where apply (ArrowF f) = f

-----

newtype Dual cat a b = Dual (cat b a)

instance Cat.Category cat => Cat.Category (Dual cat) where
  id = Dual Cat.id
  Dual f . Dual g = Dual (g Cat.. f)

instance (Apply (cat b a) dom cod) => Apply (Dual cat a b) dom cod where apply (Dual f) = apply f

-----

data ToDual (cat :: Type -> Type -> Type) = ToDual

instance (dom ~ cat b a, cod ~ Dual cat a b) => Apply (ToDual cat) dom cod where apply _ = Dual

-----

data Compose fun2 fun1 = Compose fun2 fun1

instance
  (
    Apply fun1 dom x
  ,
    Apply fun2 x cod
  )
  =>
  Apply (Compose fun2 fun1) dom cod
  where
    apply (Compose fun2 fun1) = apply fun2 . apply fun1

-----

data ToArrowA (f :: Type -> Type) = ToArrowA

-- | The effects of the leftmost 'ArrowF' happen first
instance
  (
    Applicative f
  ,
    dom ~ f (a -> b)
  )
  =>
  Apply (ToArrowA f) dom (ArrowF f a b)
  where
    apply _ ff = ArrowF (\fa -> ff <*> fa)

-----
