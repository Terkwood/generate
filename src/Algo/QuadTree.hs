module Algo.QuadTree
  ( QuadTree(..)
  , Quad(..)
  , Leaf(..)
  , new
  , insert
  , nearest
  , empty
  ) where

import Linear

import qualified Data.List as L
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V
import Geom.Rect
import Test.Hspec

data QuadTree v
  = LeafNode Rect
             (Leaf v)
  | QuadNode (Quad v)
  deriving (Eq, Show)

data Leaf v = Leaf
  { leafPosition :: V2 Double
  , leafTag :: v
  } deriving (Eq, Show)

data Quad v = Quad
  { quadRegion :: Rect
  , quadRepresentative :: Maybe (Leaf v)
  , quadChildren :: ( Maybe (QuadTree v)
                    , Maybe (QuadTree v)
                    , Maybe (QuadTree v)
                    , Maybe (QuadTree v))
  } deriving (Eq, Show)

data Quadrant
  = Q1
  | Q2
  | Q3
  | Q4
  deriving (Eq, Show)

-- Returns a new QuadTree representing the given Rect.
new :: Rect -> QuadTree v
new rect = QuadNode $ newQuad rect

-- Returns (True, Tree with leaf inserted) if the leaf position is within
-- the domain of the quadtree. Returns (False, Tree unchanged otherwise).
insert :: Leaf v -> QuadTree v -> (Bool, QuadTree v)
insert leaf@(Leaf position _) tree =
  if withinRect (_region tree) position
    then (True, ) $ _insert leaf tree
    else (False, tree)

_region :: QuadTree v -> Rect
_region (QuadNode (Quad reg _ _)) = reg
_region (LeafNode reg _) = reg

_insert :: Leaf v -> QuadTree v -> QuadTree v
_insert leaf@(Leaf p _) (QuadNode quad@(Quad reg _ _)) =
  let q = _quadrantOf reg p
   in QuadNode $ _updateChild quad leaf q
_insert leaf@(Leaf p _) (LeafNode reg oldLeaf) =
  let branch = new reg
   in _insertMany [oldLeaf, leaf] branch

_insertMany :: [Leaf v] -> QuadTree v -> QuadTree v
_insertMany (leaf:leaves) tree = _insertMany leaves $ _insert leaf tree
_insertMany [] tree = tree

_updateChild :: Quad v -> Leaf v -> Quadrant -> Quad v
_updateChild (Quad reg rep (c1, c2, c3, c4)) leaf q =
  let subRegion = _subRegion reg q
      fallback = LeafNode subRegion leaf
      (constructor, child) =
        case q of
          Q1 -> (\c -> (c, c2, c3, c4), c1)
          Q2 -> (\c -> (c1, c, c3, c4), c2)
          Q3 -> (\c -> (c1, c2, c, c4), c3)
          Q4 -> (\c -> (c1, c2, c3, c), c4)
      child' = fromMaybe fallback $ fmap (_insert leaf) child
      rep' = fromMaybe leaf rep
   in Quad reg (Just rep') $ constructor $ Just child'

_subRegion :: Rect -> Quadrant -> Rect
_subRegion (Rect (V2 tlx tly) w h) q =
  case q of
    Q1 -> Rect (V2 (tlx + w / 2) (tly + h / 2)) (w / 2) (h / 2)
    Q2 -> Rect (V2 tlx (tly + h / 2)) (w / 2) (h / 2)
    Q3 -> Rect (V2 tlx tly) (w / 2) (h / 2)
    Q4 -> Rect (V2 (tlx + w / 2) tly) (w / 2) (h / 2)

_quadrantOf :: Rect -> V2 Double -> Quadrant
_quadrantOf r (V2 x y) =
  let (V2 cx cy) = rectCenter r
   in if x >= cx
        then if y >= cy
               then Q1
               else Q4
        else if y >= cy
               then Q2
               else Q3

-- Returns the nearest point in the QuadTree to the given point.
nearest :: V2 Double -> QuadTree v -> Maybe (Leaf v)
nearest p tree = _nearest (_treeLeaf tree) p $ V.fromList [tree]

_nearest ::
     (Maybe (Leaf v)) -> V2 Double -> V.Vector (QuadTree v) -> Maybe (Leaf v)
_nearest best p trees =
  let treesWithLeaves = V.filter (isJust . _treeLeaf) trees
      candidateLeaves = V.mapMaybe (_treeLeaf) treesWithLeaves
      candidateLeaves' =
        case best of
          Just best -> V.cons best $ candidateLeaves
          Nothing -> candidateLeaves
      best' =
        if V.null candidateLeaves'
          then Nothing
          else Just $ V.minimumBy (comparing (_leafDistance p)) candidateLeaves'
   in case best' of
        Nothing -> Nothing
        Just best' ->
          let eligible = V.filter (_eligible p best') treesWithLeaves
              next = V.concatMap (_nextLevel) eligible
           in if V.null next
                then Just best'
                else _nearest (Just best') p next

_treeLeaf :: QuadTree v -> Maybe (Leaf v)
_treeLeaf (QuadNode (Quad _ rep _)) = rep
_treeLeaf (LeafNode _ leaf) = Just leaf

_leafDistance :: V2 Double -> Leaf v -> Double
_leafDistance p c1@(Leaf p1 _) = distance p p1

-- Returns True iff the QuadTree contains no points.
empty :: QuadTree v -> Bool
empty (QuadNode (Quad _ rep _)) = isNothing rep
empty (LeafNode _ _) = False

-- A tree is still eligible for search (as in, may contain a closer leaf than
-- the one given) if its region is closer to the point than the given leaf
-- leaf point and it contains at least one point.
_eligible :: V2 Double -> Leaf v -> QuadTree v -> Bool
_eligible p (Leaf leafPos _) tree =
  case _distanceToNode p tree of
    Just dist -> dist <= distance p leafPos
    Nothing -> False

_nextLevel :: QuadTree v -> V.Vector (QuadTree v)
_nextLevel (QuadNode (Quad _ _ (c1, c2, c3, c4))) =
  V.fromList $ catMaybes [c1, c2, c3, c4]
_nextLevel (LeafNode _ (Leaf _ _)) = V.empty

_distanceToNode :: V2 Double -> QuadTree v -> Maybe (Double)
_distanceToNode p (QuadNode (Quad rect rep _)) =
  case rep of
    Just _ -> Just $ distanceToRect rect p
    Nothing -> Nothing
_distanceToNode p (LeafNode _ (Leaf leaf _)) = Just $ distance p leaf

newQuad :: Rect -> Quad v
newQuad rect = Quad rect Nothing (Nothing, Nothing, Nothing, Nothing)
