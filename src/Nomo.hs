{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Nomo (module Nomo) where   -- TODO explicit list

import qualified Control.Applicative as App
import qualified Control.Arrow as Arrow
import qualified Control.Category as Cat
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.SOP.BasicFunctors as SOP
import qualified Data.SOP.NP as SOP
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified GHC.Arr as Arr
import qualified Nomo.Control.Apply as A
import qualified Nomo.Data.Vec as V

import qualified Nomo.Acc as Acc
import qualified Nomo.Class as Class

nomo :: Class.Stopped a -> a
nomo = Class.getStopped

--------------------------------------------------------------------------------
-- TODO

-- juxtapositions as 'Control.Arrow.+++'
-- juxtapositions as 'Control.Arrow.***'
-- 'Control.Arrow.+++' for n-ary sums (cf @anonymous-sums@ package)
-- 'Control.Arrow.***' for n-ary tuples
-- 'Control.Arrow.&&&'
-- 'IntMap'
-- 'IntSet'
-- 'seq'
-- 'sequence'
-- 'traverse'
-- 'asum'
-- 'many'
-- 'some'
-- @*Map@
-- left-folds for Monoid?
-- left-folds for Semigroup?

--------------------------------------------------------------------------------
-- Foldl

-- | Interpret juxtapositions as the given left-associative function,
-- with an initial value
foldl ::
    Class.Steps (Acc.Foldl a b) funs
  =>
  (b -> a -> b) ->
  b ->
  funs
foldl = \f z -> Class.steps $ Acc.foldl f z

-- | Interpret juxtapositions as the given left-associative function,
-- requiring at least one element
foldl1 ::
    Class.Steps (Acc.Foldl a a) funs
  =>
  (a -> a -> a) ->
  a ->
  funs
foldl1 = Nomo.foldl

--------------------------------------------------------------------------------
-- Foldr

-- | Interpret juxtapositions as the given right-associative function,
-- with a sentinel value
foldr ::
    Class.Steps (Acc.Foldr a b) funs
  =>
  (a -> b -> b) ->
  b ->
  funs
foldr = \f z -> Class.steps $ Acc.foldr f z

-- | A generalization of 'foldr1' for which the combining function has
-- a less constrained type
bootstrap_foldr1 ::
    Class.Steps (Acc.Foldr1 a b) funs
  =>
  (a -> b -> b) ->
  (a -> b) {- ^ How to bootstrap from the last value -} ->
  a ->
  funs
bootstrap_foldr1 = \f zf latest -> Class.steps $ Acc.foldr1 f zf latest

-- | Interpret juxtapositions as the given right-associative function,
-- requiring at least one element
--
-- Also see 'bootstrap_foldr1'.
foldr1 ::
    Class.Steps (Acc.Foldr1 a a) funs
  =>
  (a -> a -> a) ->
  a ->
  funs
foldr1 = \f latest -> bootstrap_foldr1 f id latest

--------------------------------------------------------------------------------
-- Monoid

-- | Interpret juxtapositions as '<>', with a rightmost 'mempty'
fold ::
  forall m funs.
  (
    Monoid m
  ,
    Class.Steps (Acc.Foldr m m) funs
  )
  =>
  funs
fold = Nomo.foldMap (id :: m -> m)

-- | Interpret juxtapositions as '<>', with a rightmost 'mempty', and
-- pre-mapping each argument to the monoid
foldMap ::
  (
    Monoid m
  ,
    Class.Steps (Acc.Foldr a m) funs
  )
  =>
  (a -> m) ->
  funs
foldMap = \f -> Nomo.foldr ((<>) . f) mempty

--------------------------------------------------------------------------------
-- Semigroup

-- | Interpret juxtapositions as '<>', requiring at least one element
fold1 ::
  forall m funs.
  (
    Semigroup m
  ,
    Class.Steps (Acc.Foldr1 m m) funs
  )
  =>
  m ->
  funs
fold1 = foldMap1 (id :: m -> m)

-- | Interpret juxtapositions as '<>', requiring at least one element,
-- pre-mapping each argument to the semigroup
foldMap1 ::
  (
    Semigroup m
  ,
    Class.Steps (Acc.Foldr1 a m) funs
  )
  =>
  (a -> m) ->
  a ->
  funs
foldMap1 = \f latest -> Nomo.bootstrap_foldr1 ((<>) . f) f latest

--------------------------------------------------------------------------------
-- @base@ container types

-- | Interpret juxtapositions as '(:)' with a sentinel '[]'
list ::
  forall a funs.
    Class.Steps (Acc.Foldr a [a]) funs
  =>
  funs
list = Nomo.foldr (:) ([] @a)

-- | As 'Nomo.list', requiring at least one element
list1 ::
    Class.Steps (Acc.Foldr a [a]) funs
  =>
  a ->
  funs
list1 = Nomo.list

-- | Interpret juxtapositions as 'NE.cons', pre-mapping the final
-- element with @('NE.:|' '[]')@
nonEmpty ::
  forall a funs.
    Class.Steps (Acc.Foldr1 a (NE.NonEmpty a)) funs
  =>
  a ->
  funs
nonEmpty = Nomo.bootstrap_foldr1 NE.cons (NE.:| [])

-- | As 'Nomo.list', post-mapping 'App.ZipList'
zipList ::
  forall a funs.
    Class.Steps (Acc.Post [a] (App.ZipList a) (Acc.Foldr a [a])) funs
  =>
  funs
zipList = Class.steps $ Acc.post (App.ZipList @a) $ Acc.list @a

-- | As 'Nomo.list1', post-mapping 'App.ZipList'
zipList1 ::
    Class.Steps (Acc.Post [a] (App.ZipList a) (Acc.Foldr a [a])) funs
  =>
  a ->
  funs
zipList1 = Nomo.zipList

-- | As 'Nomo.list', post-mapping @'Arr.listArray' bounds@
listArray ::
  forall i e funs.
  (
    Arr.Ix i
  ,
    Class.Steps (Acc.Post [e] (Arr.Array i e) (Acc.Foldr e [e])) funs
  )
  =>
  (i,i) ->
  funs
listArray =
    \bounds ->
        Class.steps
      $ Acc.post (Arr.listArray @i @e bounds)
      $ Acc.list @e

-- | As 'Nomo.listArray', requiring at least one element
listArray1 ::
  forall i e funs.
  (
    Arr.Ix i
  ,
    Class.Steps (Acc.Post [e] (Arr.Array i e) (Acc.Foldr e [e])) funs
  )
  =>
  (i,i) ->
  e ->
  funs
listArray1 = Nomo.listArray

-- | Interpet juxtapositions as 'Nomo.Data.Vec.VCons', with a sentinel
-- 'Nomo.Data.Vec.VNil'
vec ::
  forall a n funs.
    Class.Steps (Acc.Vec n n a) funs
  =>
  funs
vec = Class.steps $ Acc.vec @n @a

-- | Interpet juxtapositions as 'Nomo.Data.Vec.VCons', with a sentinel
-- 'Nomo.Data.Vec.VNil', pre-mapping the given function
vecMap ::
  forall a b n funs.
    Class.Steps (Acc.Pre a b (Acc.Vec n n b)) funs
  =>
  (a -> b) ->
  funs
vecMap f = Class.steps $ Acc.pre f $ Acc.vec @n @b

--------------------------------------------------------------------------------
-- @containers@ container types

-- | Interpret alternating juxtapositions as @'uncurry' 'Map.insert'@
-- and '(,)', with a trailing sentinel 'Map.empty'
map ::
  forall k v funs.
    Class.Steps (Acc.Map k v) funs
  =>
  funs
map = Class.steps $ Acc.map @k @v

-- | Interpret juxtapositions as 'Set.insert', with a trailing
-- sentinel 'Set.empty'
set ::
  forall a funs.
  (
    Ord a
  ,
    Class.Steps (Acc.Foldl a (Set.Set a)) funs
  )
  =>
  funs
set = Nomo.foldl (\acc a -> Set.insert (a :: a) acc) Set.empty

-- | Interpret juxtapositions as 'Seq.cons' with a trailing sentinel
-- 'Seq.empty'
sequ ::
  forall a funs.
  (
    Ord a
  ,
    Class.Steps (Acc.Foldl a (Seq.Seq a)) funs
  )
  =>
  funs
sequ = Nomo.foldl (Seq.|>) (Seq.empty :: Seq.Seq a)

-- | Interpret juxtapositions as @'uncurry' 'Data.Map.insert'@, with a
-- trailing sentinel 'Map.empty'
mapPairs ::
  forall k v funs.
  (
    Ord k
  ,
    Class.Steps (Acc.Foldl (k,v) (Map.Map k v)) funs
  )
  =>
  funs
mapPairs = Nomo.foldl (\acc (k,v) -> Map.insert (k :: k) (v :: v) acc) Map.empty

--------------------------------------------------------------------------------
-- Tuples

-- | Interpret juxtapositions as commas, ultimately invoking the
-- corresponding tuple type, requiring at least two arguments
tuple ::
    Class.Steps (Acc.Tuple (a,b)) funs
  =>
  a ->
  b ->
  funs
tuple a b = Class.steps $ Acc.tuple (a,b)

--------------------------------------------------------------------------------
-- Pure functions

-- | Interpret juxtapositions as 'Cat..'
compose ::
  forall cat a funs.
  (
    Cat.Category cat
  ,
    Class.Steps (Acc.Compose cat a a) funs
  )
  =>
  funs
compose = Class.steps $ Acc.compose @cat @a

-- | Interpret leading juxtapositions as 'Cat..' and the final
-- juxtaposition as 'A.apply'
app ::
  forall cat b a r funs.
    Class.Steps (Acc.App A.Id cat b a r) funs
  =>
  cat b r ->
  a ->
  funs
app f a = Class.steps $ Acc.app A.Id f a

-- | Interpret juxtapositions as @\a f -> 'apply' f a@ (ie
-- left-associative)
rapp ::
  forall a funs.
    Class.Steps (Acc.RApp a) funs
  =>
  a ->
  funs
rapp a = Class.steps $ Acc.rapp a

--------------------------------------------------------------------------------
-- Monadic functions

-- | Interpret juxtapositions as '>>='
bind ::
  forall m a funs.
    Class.Steps (Acc.Bind m a) funs
  =>
  m a ->
  funs
bind m = Class.steps $ Acc.bind m

-- | Interpret leading juxtapositions as 'Control.Monad.<=<' and the
-- final juxtaposition as 'Control.Monad.=<<' (aka " reverse bind ",
-- aka " monadic application ")
rbind ::
  forall m b a r funs.
  (
    Monad m
  ,
    Class.Steps (Acc.App (A.ToArrowM m) (A.ArrowF m) b a r) funs
  )
  =>
  (b -> m r) ->
  a ->
  funs
rbind f a = Class.steps $ Acc.app (A.ToArrowM @m) (A.apply A.ToArrowM f) a

-- | Interpret juxtapositions as 'Control.Monad.<=<'
kleisli ::
  forall m a b funs.
  (
    Monad m
  ,
    Class.Steps
      ( Acc.Post
          (Arrow.Kleisli m a b)
          (a -> m b)
          ( Acc.PrePoly
              (A.ToKleisli m)
              (Acc.Compose (Arrow.Kleisli m) b b)
          )
      )
      funs
  )
  =>
  funs
kleisli =
    Class.steps
  $ Acc.post (Arrow.runKleisli @m @a @b)
  $ Acc.prePoly (A.ToKleisli @m)
  $ Acc.compose @(Arrow.Kleisli m) @b

--------------------------------------------------------------------------------
-- Applicative functions

-- | Interpret juxtapositions as '<*>'
splat ::
  forall f a funs.
    Class.Steps (Acc.Splat f a) funs
  =>
  f a ->
  funs
splat fa = Class.steps $ Acc.splat fa

-- | As 'splat', except the first juxtaposition is '<$>'
--
-- The idiom bracket notation from McBride and Paterson (2008)
-- <http://www.staff.city.ac.uk/~ross/papers/Applicative.html>
idiom ::
  forall f a b funs.
  (
    Applicative f
  ,
    Class.Steps (Acc.Splat f (b -> a)) funs
  )
  =>
  (b -> a) ->
  funs
idiom f = Class.steps $ Acc.splat (pure @f f)

-- | Interpret juxtapositions as a right-associative '<*>'
splatr ::
  forall f b a r funs.
  (
    Applicative f
  ,
    Class.Steps (Acc.App (A.ToArrowA f) (A.ArrowF f) b a r) funs
  )
  =>
  f (b -> r) ->
  a ->
  funs
splatr f a = Class.steps $ Acc.app (A.ToArrowA @f) (A.apply A.ToArrowA f) a

-- | Interpret juxtapositions as a left-associative operator equal to @'flip' '(<*>)'@
rsplat ::
  forall f a r funs.
  (
    Applicative f
  ,
    Class.Steps (Acc.RSplat f r a r) funs
  )
  =>
  f a ->
  funs
rsplat fa = Class.steps (Acc.rsplat fa :: Acc.RSplat f r a r)

-- | Interpret juxtapositions as a right-associative operator equal to @'flip' '(<*>)'@
rsplatr ::
  forall f a funs.
  (
    Applicative f
  ,
    Class.Steps (Acc.RSplatr f a) funs
  )
  =>
  f a ->
  funs
rsplatr fa = Class.steps $ Acc.rsplatr fa

--------------------------------------------------------------------------------
-- @sop-core@ types

-- | Interpret juxtapositions as 'SOP.:*', with a sentinel 'SOP.Nil'
np ::
  forall k (f :: k -> Type) xs funs.
    Class.Steps (Acc.NP f xs xs) funs
  =>
  funs
np = Class.steps $ Acc.np @k @f @xs

-- | Interpret juxtapositions as a left-associative operator equal to
-- @'flip' '(SOP.:*)'@, with an initial 'SOP.Nil'
npr ::
  forall k (f :: k -> Type) funs.
    Class.Steps (Acc.NPr f '[]) funs
  =>
  funs
npr = Class.steps $ Acc.npr @k @f

-- | Interpret juxtapositions as 'SOP.:*', with a sentinel 'SOP.Nil'
-- and pre-mapping 'SOP.I'
inp ::
  forall xs funs.
    Class.Steps (Acc.PrePoly A.SopI (Acc.NP SOP.I xs xs)) funs
  =>
  funs
inp = Class.steps $ Acc.prePoly A.SopI $ Acc.np @Type @SOP.I @xs

-- | Interpret juxtapositions as 'SOP.:*'a left-associative operator
-- equal to @'flip' '(SOP.:*)'@, with an initial 'SOP.Nil', and
-- pre-mapping 'SOP.I'
inpr ::
  forall funs.
    Class.Steps (Acc.PrePoly A.SopI (Acc.NPr SOP.I '[])) funs
  =>
  funs
inpr = Class.steps $ Acc.prePoly A.SopI $ Acc.npr @Type @SOP.I
