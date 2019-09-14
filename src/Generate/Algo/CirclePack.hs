module Generate.Algo.CirclePack
  ( CircleSearch(..)
  , search
  , mkSearch
  ) where

import Data.List
import Data.Ord

import qualified Generate.Algo.QuadTree as Q
import Generate.Geom.Circle
import Generate.Geom.Rect
import Generate.Monad

data CircleSearch =
  CircleSearch
    { circleSearchTree :: Q.QuadTree Circle
    , circleSearchFoundCircles :: [Circle]
    , circleSearchRemainingAttempts :: Int
    , circleSearchSearchPattern :: Generate Circle
    }

mkSearch :: Int -> Rect -> Generate Circle -> CircleSearch
mkSearch n frame pattern =
  CircleSearch
    (Q.newWithNodeUpdater representativeNodeUpdater frame)
    []
    n
    pattern
  where
    leafRadius :: Q.Leaf Circle -> Double
    leafRadius = (\(Circle _ r) -> r) . Q.leafTag
    representativeNodeUpdater ::
         Maybe (Q.Leaf Circle) -> Q.Leaf Circle -> Maybe (Q.Leaf Circle)
    representativeNodeUpdater current new =
      case current of
        Just current -> Just $ maximumBy (comparing leafRadius) [new, current]
        Nothing -> Just new

data CircleSearchHeuristic =
  CircleSearchHeuristic

instance Q.Heuristic CircleSearchHeuristic Circle where
  distanceBetween _ (Q.Leaf _ c1) (Q.Leaf _ c2) = negate $ overlap c1 c2
  eligible _ (Q.Leaf _ best) (Q.Leaf p search@(Circle _ sr)) (Q.Quad _ reg rep _) =
    case rep of
      Just (Q.Leaf _ (Circle _ cr)) ->
        let distanceToRegion = distanceToRect reg p
            distanceToClosestPossible = distanceToRegion - cr - sr
         in distanceToClosestPossible <
            ((negate $ overlap best search) :: Double)
      Nothing -> False

valid :: Q.QuadTree Circle -> Circle -> Bool
valid tree c@(Circle center _) =
  let leaf = Q.nearestBy CircleSearchHeuristic tree $ Q.Leaf center c
   in case leaf of
        Just (Q.Leaf _ nearestCircle) -> not $ overlap c nearestCircle > 0
        Nothing -> True

{- Usage: iterateMaybeM search <searchObj> >>= return . last -}
search :: CircleSearch -> Generate (Maybe CircleSearch)
search (CircleSearch _ _ 0 _) = return Nothing
search s@(CircleSearch tree circles remaining pattern) = do
  let remaining' = remaining - 1
  candidate@(Circle center _) <- pattern
  if valid tree candidate
    then return $
         Just $
         CircleSearch
           (snd $ Q.insert tree $ Q.Leaf center candidate)
           (candidate : circles)
           remaining'
           pattern
    else return $ Just $ s {circleSearchRemainingAttempts = remaining'}
