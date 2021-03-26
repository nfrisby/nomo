{-# OPTIONS_GHC -Wmissing-signatures #-}

-- Comment to see values.
-- Uncomment to see inferred types.
-- {-# OPTIONS_GHC -w #-}

module Nomo.Examples (main) where

import           Nomo (nomo)
import qualified Nomo
import qualified Nomo.Acc
import qualified Nomo.Class

list1 () = nomo $ Nomo.list 1 2 3
list2 () = nomo $ Nomo.list 1 2 3 4
list3 () = nomo $ Nomo.list

fun1 () = nomo $ Nomo.compose (/2) (+2) (*3)
fun2 () = nomo $ Nomo.app     (/2) (+2) (*3) 7
fun3 () = nomo $ Nomo.kleisli      (pure . show) (pure . (*2)) (\x -> [x, x+3]) (pure . (*4))
fun4 () = nomo $ Nomo.bind [1,2,3] (pure . (*4)) (\x -> [x, x+3]) (pure . (*2)) (pure . show)
fun5 () = nomo $ Nomo.rbind        (pure . show) (pure . (*2)) (\x -> [x, x+3]) (pure . (*4)) [1,2,3]
fun6 () = nomo $ Nomo.splat        [\f g i -> f (g i)]               [id,reverse] [show] [123,456]
fun7 () = nomo $ Nomo.splat        [\f g i -> nomo $ Nomo.app f g i] [id,reverse] [show] [123,456]
fun8 () = nomo $ Nomo.idiom        (\f g i -> f (g i))               [id,reverse] [show] [123,456]
fun9 () = nomo $ Nomo.splatr                                         [id,reverse] [show] [123,456]
fun10 () = nomo $ Nomo.rsplatr [123,456] [show] [id,reverse]
fun11 () = nomo $ Nomo.rsplat  [123,456] [show] [id,reverse] [\f g i -> f (g i)]
fun12 () = nomo $ Nomo.rapp    7 (*3) (+2) (/2) show

tuple1 () = nomo $ Nomo.tuple 1 "OK"
tuple2 () = nomo $ Nomo.tuple 1 "OK" True
tuple3 () = nomo $ Nomo.tuple
  01 02 03 04 05
  06 07 08 09 10
  11 12 13 14 15
  16 17 18 19 20
  21 22 23 24 25

  26 27 28 29 30
  31 32 33 34 35
  36 37 38 39 40
  41 42 43 44 45
  46 47 48 49 50

  51 52 53 54 55
  56 57 58 59 60
  61 62   -- GHC stops at 62-tuples, so @nomo@ does too

vec1 () = nomo $ Nomo.vec 10 20 30
vec2 () = nomo $ Nomo.vec
  (nomo $ Nomo.vec 1 5)
  (nomo $ Nomo.vec 2 6)
  (nomo $ Nomo.vec 3 7)
  (nomo $ Nomo.vecMap (+3) 1 5)

vec3 () = nomo $ Nomo.vec
  (nomo $ Nomo.vec 1 5)
  (nomo $ Nomo.vec 2 6)
  (nomo $ Nomo.vec 3 7)
  (nomo $ Nomo.vecMap (+3) 1 5)

main :: IO ()
main = do
  putStrLn "\n-- Lists"
  print $ list1 ()
  print $ list2 ()

  putStrLn "\n-- Functions"
  print $ fun1 () $ 7
  print $ fun2 ()
  print $ [1,2,3] >>= fun3 () 
  print $ fun4 ()
  print $ fun5 ()
  print $ fun6 ()
  print $ fun7 ()
  print $ fun8 ()
  print $ fun9 ()
  print $ fun10 ()
  print $ fun11 ()
  print $ fun12 ()

  putStrLn "\n-- Order of Applicative effects"
  do
    let
      fshow :: IO (Int -> String)
      fshow = show <$ putStr "a"

      fdbl :: IO (Int -> Int)
      fdbl = (*2) <$ putStr "b"

      f3 :: IO Int
      f3 = 3 <$ putStr "c"

      assoc a b c = a (b c)
      done = (>>= putStrLn)
    done $ nomo $ Nomo.splat   (pure assoc) fshow fdbl f3
    done $ nomo $ Nomo.splatr               fshow fdbl f3
    done $ nomo $ Nomo.rsplat               f3 fdbl fshow (pure assoc)
    done $ nomo $ Nomo.rsplatr              f3 fdbl fshow

  putStrLn "\n-- Tuples"
  print $ tuple1 ()
  print $ tuple2 ()

  putStrLn "\n-- Vectors"
  print $ vec1 ()
  print $ vec2 ()

  putStrLn "\n-- Containers"
  print $ nomo $ Nomo.set 1 3 2 3
  print $ nomo $ Nomo.sequ 1 2 3
  print $ nomo $ Nomo.map       2 "like"   1 "magic"
  print $ nomo $ Nomo.mapPairs (2,"more") (1,"normal")

  putStrLn "\n-- NonEmpty"
  print $ nomo $ Nomo.nonEmpty ()
  print $ nomo $ Nomo.nonEmpty 1 2 3 4 5

  putStrLn "\n-- Sums"
  print $ sum $ list3 ()
  print $ sum $ nomo $ Nomo.Class.steps (Nomo.Acc.pre (`mod` 4) $ Nomo.Acc.list) 11 20 33
  print $ sum $                                   map (`mod` 4) $       [11, 20, 33]

  putStrLn "\n-- Data.SOP.NP"
  print $ nomo $ Nomo.np (Just True) (Just ()) (pure 3)
  print $ nomo $ Nomo.npr (pure 3) (Just ()) (Just True)
  print $ nomo $ Nomo.inp True () 3
  print $ nomo $ Nomo.inpr 3 () True

-- Should give a very good error message
-- bad () = Nomo.tuple 1 "OK" True

{- Output with `bad' uncommented

src/Nomo/Examples.hs:117:1-31: error:
    • Did you forget to apply `nomo'?
      
          An application of a variadic function must be immediately passed to `nomo',
          as in the following examples.
              `nomo (foo a b c d e)'
              `nomo (foo u v)'
              `nomo $ foo x y z'
          If not all arguments are present yet, eta-expand in order to bind the
          deferred arguments in a lambda, as in the following example.
              `\p q -> nomo (foo a p b q c)'
      
      Lower-level details:
          This error arose because the code tried to interpret a stepping
          accumulator of type
      
              Nomo.Acc.Tuple (t, [Char])
      
          as a function of this type
      
              Bool -> t1
      
          Either fix the error locally, or if you're defining your own variadic
          function, check if you need to annotate polymorphic values (eg the empty
          list) in your definition with type variables and that your signatures
          provides *exactly* the `Nomo.Class.Steps' constraint that your
          `Nomo.Class.steps` application needs.
      
    • When checking the inferred type
        bad :: forall t1 t2. ((TypeError ...), Num t1) => () -> t2
    |
117 | bad () = Nomo.tuple 1 "OK" True

-}
