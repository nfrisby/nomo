{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Type-level variadic contexts.
module Nomo.Type (
    Context,
    C1,
  ) where

import qualified Data.Kind as K
import           GHC.TypeLits

-- | A variadic 'K.Constraint' constructor
--
-- WARNING: Use of 'Context' within the instance context of a class
-- with functional dependences may cause GHC's check of the /liberal/
-- /coverage/ /condition/ to fail.
--
-- NOTE: Unlike the value level variadic functions, you do not need to
-- wrap 'Context' in an application of something analogous to
-- 'Nomo.nomo'. The built-in @=>@ syntax serves the same purpose.
type family Context :: k where
  Context = C0
  Context = C1
  Context = C2
  Context = C3
  Context = C4
  Context = C5
  Context = C6
  Context = C7
  Context = C8
  Context = C9
  Context = C10
  Context = C11
  Context = TypeError (Text "Nomo.Type.Context cannot handle more than 11 arguments. Chain them by using it infixl: " :$$: Text "    Context a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 `Context` a11 a12 a13 a14 ..." ) :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9 -> k10 -> k11 -> k
  Context = TypeError
    (Text ""
     :<>: Text "Nomo.Type.Context is a variadic constructor for `Constraint'."
     :$$: Text "It cannot occur at kind " :<>: ShowType k :<>: Text "."
     :$$: Text "If the above kind involves type variables, consider adding an annotation `Context ... :: " :<>: ShowType K.Constraint :<>: Text "'."
    ) :: k

--------------------------------------------------------------------------------
--
-- Context auxiliaries

infixl 0 `Context`

type ToC (c :: k) = (k ~ K.Constraint, ToC_ c)
type family ToC_ (c :: k) :: K.Constraint where ToC_ c = c

{-
  flip mapM_ [0 .. 11] $ \n -> do
    let context = "(" <> context' <> ")"
        context' =
          intercalate "," [ "ToC a" <> show i | i <- [ 1 .. n ] ]
        args =
          concat [ " (a" <> show i <> " :: k" <> show i <> ")" | i <- [ 1 .. n ] ]
    putStrLn $ "instance " <> context <> " => C" <> show n <> args
-}

class () => C0
class (ToC a1) => C1 (a1 :: k1)
class (ToC a1,ToC a2) => C2 (a1 :: k1) (a2 :: k2)
class (ToC a1,ToC a2,ToC a3) => C3 (a1 :: k1) (a2 :: k2) (a3 :: k3)
class (ToC a1,ToC a2,ToC a3,ToC a4) => C4 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4)
class (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5) => C5 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5)
class (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6) => C6 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6)
class (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7) => C7 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7)
class (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8) => C8 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8)
class (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8,ToC a9) => C9 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8) (a9 :: k9)
class (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8,ToC a9,ToC a10) => C10 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8) (a9 :: k9) (a10 :: k10)
class (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8,ToC a9,ToC a10,ToC a11) => C11 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8) (a9 :: k9) (a10 :: k10) (a11 :: k11)

instance () => C0
instance (ToC a1) => C1 (a1 :: k1)
instance (ToC a1,ToC a2) => C2 (a1 :: k1) (a2 :: k2)
instance (ToC a1,ToC a2,ToC a3) => C3 (a1 :: k1) (a2 :: k2) (a3 :: k3)
instance (ToC a1,ToC a2,ToC a3,ToC a4) => C4 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4)
instance (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5) => C5 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5)
instance (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6) => C6 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6)
instance (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7) => C7 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7)
instance (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8) => C8 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8)
instance (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8,ToC a9) => C9 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8) (a9 :: k9)
instance (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8,ToC a9,ToC a10) => C10 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8) (a9 :: k9) (a10 :: k10)
instance (ToC a1,ToC a2,ToC a3,ToC a4,ToC a5,ToC a6,ToC a7,ToC a8,ToC a9,ToC a10,ToC a11) => C11 (a1 :: k1) (a2 :: k2) (a3 :: k3) (a4 :: k4) (a5 :: k5) (a6 :: k6) (a7 :: k7) (a8 :: k8) (a9 :: k9) (a10 :: k10) (a11 :: k11)
