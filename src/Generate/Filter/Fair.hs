module Generate.Filter.Fair
  ( fairFilter
  ) where

import Control.Monad.Extra

import Generate.Monad

-- For each element, evaluates the predicate. Retains the
-- element if the predicate is true.
fairFilter :: Generate Bool -> [a] -> Generate [a]
fairFilter predicate as = mapMaybeM (test predicate) as

test :: Generate Bool -> a -> Generate (Maybe a)
test predicate a =
  predicate >>= \allowed ->
    return $
    if allowed
      then Just a
      else Nothing
