{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Nomo.Data.Vec (
    Vec (..),
    toList,
  ) where

import           Data.Kind (Type)
import           GHC.TypeLits

-- | This /belongs/ in ~~a museum~~ the @base@ libraries!
data Vec :: Nat -> Type -> Type where
  Cons :: a -> Vec (n - 1) a -> Vec n a
  Nil  ::                       Vec 0 a

instance Show a => Show (Vec n a) where show = show . toList

toList :: Vec n a -> [a]
toList = \case
  Cons a v -> a : toList v
  Nil      -> []
