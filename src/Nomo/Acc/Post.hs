{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nomo.Acc.Post (
    Post,
    post,
  ) where

import           Nomo.Class

data Post a b acc = Post (a -> b) !acc

infixl `post`

post :: (a -> b) -> acc -> Post a b acc
post = Post

instance
  (
    x ~ b
  ,
    Stop acc a
  )
  =>
  Stop (Post a b acc) x
  where
    stop (Post f acc) = f (stop acc)

instance
    Step acc x acc'
  =>
  Step (Post a b acc) x (Post a b acc')
  where
    step (Post f acc) a = Post f (step acc a)
