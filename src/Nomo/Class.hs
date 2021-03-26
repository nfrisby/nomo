{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Nomo.Class (
    -- * Iteration operations
    Step (step),
    Stop (stop),

    -- * Variadic application
    Steps,
    Stopped (getStopped),
    steps,
  ) where

import           GHC.TypeLits

--------------------------------------------------------------------------------
-- We use this newtype to emphasize a fundamental transition from
-- 'step'ing to 'stop'ped.

-- | The output of 'stop', and so the output of 'steps'.
newtype Stopped a = Stopped {getStopped :: a}

instance Monad       Stopped where Stopped a >>= k = k a
instance Functor     Stopped where fmap f (Stopped a) = Stopped (f a) 
instance Applicative Stopped where
  pure = Stopped
  Stopped f <*> Stopped a = Stopped (f a)

--------------------------------------------------------------------------------
-- We define accumulators by instantiating these classes.

-- | Step from @acc@ to @acc'@ by applying an @a@.
--
-- We use a fundep instead of a type family in order to enable type
-- inference in which /some/ equalities also flow in the opposite
-- direction: from the result to the function and/or argument.
class
  Step acc a acc'
  | acc a -> acc'
  where
    step :: acc -> a -> acc'

-- | Project a value after some number of 'step's.
class
  Stop acc a
  where
    stop :: acc -> a

--------------------------------------------------------------------------------
-- Define the 'steps' function. This type class is closed; there can
-- be no more instances. Thus we don't export the class itself.

type Steps = Steps_

-- Not exported.
class
  Steps_ acc fun
  where
    steps :: acc -> fun

instance
    Stop acc a
  =>
  Steps_ acc (Stopped a)
  where
    steps = Stopped . stop

instance
  (
    Steps_ acc' b
  ,
    Step acc a acc'
  )
  =>
  Steps_ acc (a -> b)
  where
    steps acc a = steps (step acc a :: acc')

instance {-# INCOHERENT #-}
    TypeError (      Text "Did you forget to apply `nomo'?"
                :$$: Text ""
                :$$: Text "    An application of a variadic function must be immediately passed to `nomo',"
                :$$: Text "    as in the following examples."
                :$$: Text "        `nomo (foo a b c d e)'"
                :$$: Text "        `nomo (foo u v)'"
                :$$: Text "        `nomo $ foo x y z'"
                :$$: Text "    If not all arguments are present yet, eta-expand in order to bind the"
                :$$: Text "    deferred arguments in a lambda, as in the following example."
                :$$: Text "        `\\p q -> nomo (foo a p b q c)'"
                :$$: Text ""
                :$$: Text "Lower-level details:"
                :$$: Text "    This error arose because the code tried to interpret a stepping"
                :$$: Text "    accumulator of type"
                :$$: Text ""
                :$$: Text "        " :<>: ShowType acc
                :$$: Text ""
                :$$: Text "    as a function of this type"
                :$$: Text ""
                :$$: Text "        " :<>: ShowType other
                :$$: Text ""
                :$$: Text "    Either fix the error locally, or if you're defining your own variadic"
                :$$: Text "    function, check if you need to annotate polymorphic values (eg the empty"
                :$$: Text "    list) in your definition with type variables and that your signatures"
                :$$: Text "    provides *exactly* the `" :<>: ShowType Steps :<>: Text "' constraint that your"
                :$$: Text "    `Nomo.Class.steps` application needs."
                :$$: Text ""
              )
  =>
  Steps_ acc other
  where
    steps = error "unreachable code"
