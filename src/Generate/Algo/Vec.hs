module Generate.Algo.Vec
  ( sortBy
  ) where

import Data.Ord
import Data.Vector (Vector, (!), backpermute, convert, unsafeIndex)
import qualified Data.Vector.Algorithms.Intro as I
import Data.Vector.Unboxed (generate, modify)
import Prelude hiding ((!))

sortBy :: (e -> e -> Ordering) -> Vector e -> Vector e
sortBy compare vs = backpermute vs indices'
  where
    indices' = convert $ modify (I.sortBy compare') indices
    compare' i1 i2 = compare (vs ! i1) (vs ! i2)
    indices = generate (length vs) id
