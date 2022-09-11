{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Werror #-}

module Nomo.Class (
    -- * Variadic functions
    -- ** Iteration operations
    Step (step),
    Stop (stop),

    -- ** Variadic application
    Steps,
    steps,

    -- * Variadic cofunctions

    -- ** Iteration operations
    Costep (costep),
    Costop (costop),

    -- ** Variadic application
    Costeps,
    costeps,

    -- * Sentinels
    conomo,
    nomo,
  ) where

import           Data.Void (Void)
import           GHC.Exts (Proxy#, proxy#)
import           GHC.TypeLits

--------------------------------------------------------------------------------
-- Without this silly layer of indirection, GHC raises TypeError failures while
-- compiling this module

type family TypeError_ (err :: ErrorMessage) :: Void where
  TypeError_ err = TypeError err

--------------------------------------------------------------------------------
-- We use this newtype to emphasize a fundamental transition from
-- 'step'ing to 'stop'ped.

-- | The output of 'stop', and so the output of 'steps'.
newtype Stopped a = Stopped {getStopped :: a}

nomo :: Stopped a -> a
nomo = getStopped

conomo :: a -> Stopped a
conomo = Stopped

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

type Steps acc fun = Steps_ (TypeError_ (StepsErr acc fun)) acc fun

-- Not exported.
class
  Steps_ err acc fun
  where
    steps_ :: Proxy# (err :: Void) -> acc -> fun

instance
    Stop acc a
  =>
  Steps_ err acc (Stopped a)
  where
    steps_ _ = Stopped . stop

instance
  (
    Steps_ err acc' b
  ,
    Step acc a acc'
  )
  =>
  Steps_ err acc (a -> b)
  where
    steps_ err acc a = steps_ err (step acc a :: acc')

type StepsErr acc other =
         Text "Did you forget to apply `nomo'?"
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
    :$$: Text "    provides *exactly* the `Nomo.Class.Steps` constraint that your"
    :$$: Text "    application of `Nomo.Class.steps` requires."
    :$$: Text ""

steps :: forall acc fun. Steps_ (TypeError_ (StepsErr acc fun)) acc fun => acc -> fun
steps = steps_ (proxy# :: Proxy# (TypeError_ (StepsErr acc fun)))

--------------------------------------------------------------------------------
-- Define the 'costeps' function. This type class is closed; there can
-- be no more instances. Thus we don't export the class itself.

type Costeps acc res = Costeps_ (TypeError_ (CostepsErr acc res)) acc res

class
  Costep acc res a acc' res'
  | acc a -> res acc' res'
  where
    costep :: acc -> (acc' -> a -> res') -> res

class
  Costop acc a res
  where
    costop :: acc -> a -> res

-- Not exported.
class
  Costeps_ err acc fun res
  where
    costeps_ :: Proxy# (err :: Void) -> acc -> fun -> res

instance
    Costop acc a res
  =>
  Costeps_ err acc (Stopped a) res
  where
    costeps_ _err acc f = costop acc (getStopped f)

instance
  (
    Costeps_ err acc' b res'
  ,
    Costep acc res a acc' res'
  )
  =>
  Costeps_ err acc (a -> b) res
  where
    costeps_ err acc f =
      costep acc $ \acc' a -> costeps_ err acc' (f a)

type CostepsErr acc other =
         Text "Did you forget to apply `conomo'?"
    :$$: Text ""
    :$$: Text "    The argument to a variadic cofunction must immediately pass its result to `conomo',"
    :$$: Text "    as in the following examples."
    :$$: Text "        `foo $ \\a b c d e -> conomo $ ...'"
    :$$: Text "        `foo (\\u v -> conomo $ ...)'"
    :$$: Text "        `foo $ \\x y z -> conomo (...)'"
    :$$: Text "    If not all arguments are present yet, eta-expand in order to bind the"
    :$$: Text "    deferred arguments in a lambda, as in the following example."
    :$$: Text "        `foo $ \\p q -> conomo $ ... p q)'"
    :$$: Text ""
    :$$: Text "Lower-level details:"
    :$$: Text "    This error arose because the code tried to interpret a costepping"
    :$$: Text "    accumulator of type"
    :$$: Text ""
    :$$: Text "        " :<>: ShowType acc
    :$$: Text ""
    :$$: Text "    as a function of this type"
    :$$: Text ""
    :$$: Text "        " :<>: ShowType other
    :$$: Text ""
    :$$: Text "    Either fix the error locally, or if you're defining your own variadic"
    :$$: Text "    cofunction, check if you need to annotate polymorphic values (eg the empty"
    :$$: Text "    list) in your definition with type variables and that your signatures"
    :$$: Text "    provides *exactly* the `Nomo.Class.Costeps` constraint that your"
    :$$: Text "    application of `Nomo.Class.costeps` requires."
    :$$: Text ""

costeps :: forall acc fun res. Costeps_ (TypeError_ (CostepsErr acc fun)) acc fun res => acc -> fun -> res
costeps = costeps_ (proxy# :: Proxy# (TypeError_ (CostepsErr acc fun)))
