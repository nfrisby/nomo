{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.App (
    App,
    app,
  ) where

import qualified Control.Category as Cat
import           Nomo.Control.Apply (Apply (..))

import           Nomo.Class

-- | 'App' has its own injector instead of "Nomo.Acc.PrePoly" because
-- it doesn't apply the injector to the last argument.
data App inj cat b a r =
    App
      inj        -- ^ inject an argument into the category
      (cat b r)  -- ^ accumulation of injected arguments
      a          -- ^ the most recent argument

app :: inj -> cat b r -> a -> App inj cat b a r
app = App

instance
    Apply (cat b r) a x
  =>
  Stop (App inj cat b a r) x
  where
    stop (App _inj acc a) = apply acc a

instance
  (
    x ~ a2
  ,
    Cat.Category cat
  ,
    Apply inj a1 (cat b2 b1)
  )
  =>
  Step (App inj cat b1 a1 r) x (App inj cat b2 a2 r)
  where
    step (App inj acc a1) a2 = App inj (acc Cat.. apply inj a1) a2
