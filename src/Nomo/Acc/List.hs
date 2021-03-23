module Nomo.Acc.List (
    list,
  ) where

import           Nomo.Class
import           Nomo.Acc.Foldr

list :: Foldr a [a]
list = Nomo.Acc.Foldr.foldr (:) []
