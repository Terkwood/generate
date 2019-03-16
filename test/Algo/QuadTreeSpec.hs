module Algo.QuadTreeSpec
  ( spec
  ) where

import Algo.QuadTree
import Geom.Rect
import Linear
import Test.Hspec

frame = Rect (V2 0 0) 100 100

spec :: Spec
spec = foldr1 (>>) [constructSpec, insertionSpec, searchSpec]

constructSpec =
  describe "constructor" $ do
    it "yields an empty tree" $ do (empty $ new frame) `shouldBe` True

insertionSpec =
  describe "insert" $ do
    it "rejects an out of bounds leaf" $ do
      (insert (Leaf (V2 10 1000) 7) $ new frame) `shouldBe` (False, new frame)
    let expectedTreeWithOneInsertion :: QuadTree Int =
          QuadNode $
          Quad
            { quadRegion = Rect (V2 0 0) 100 100
            , quadRepresentative = Just $ Leaf (V2 10 10) 7
            , quadChildren =
                ( Nothing
                , Nothing
                , Just $ LeafNode (Rect (V2 0 0) 50 50) $ Leaf (V2 10 10) 7
                , Nothing)
            }
    let treeWithOneInsertionResult = insert (Leaf (V2 10 10) 7) $ new frame
    it "inserts a leaf" $ do
      treeWithOneInsertionResult `shouldBe` (True, expectedTreeWithOneInsertion)
    let expectedTreeWithTwoInsertions :: QuadTree Int =
          QuadNode $
          Quad
            { quadRegion = Rect (V2 0 0) 100 100
            , quadRepresentative = Just $ Leaf (V2 10 10) 7
            , quadChildren =
                ( Nothing
                , Nothing
                , Just $
                  QuadNode $
                  Quad
                    { quadRegion = Rect (V2 0 0) 50 50
                    , quadRepresentative = Just $ Leaf (V2 10 10) 7
                    , quadChildren =
                        ( Nothing
                        , Just $
                          LeafNode (Rect (V2 0 25) 25 25) (Leaf (V2 10 30) 12)
                        , Just $
                          LeafNode (Rect (V2 0 0) 25 25) (Leaf (V2 10 10) 7)
                        , Nothing)
                    }
                , Nothing)
            }
    let treeWithTwoInsertionsResult =
          insert (Leaf (V2 10 30) 12) $ snd treeWithOneInsertionResult
    it "inserts a leaf under leaf" $ do
      treeWithTwoInsertionsResult `shouldBe`
        (True, expectedTreeWithTwoInsertions)

searchSpec =
  describe "search" $ do
    let treeWithTwoLeaves :: QuadTree Int =
          QuadNode $
          Quad
            { quadRegion = Rect (V2 0 0) 100 100
            , quadRepresentative = Just $ Leaf (V2 10 10) 7
            , quadChildren =
                ( Nothing
                , Nothing
                , Just $
                  QuadNode $
                  Quad
                    { quadRegion = Rect (V2 0 0) 50 50
                    , quadRepresentative = Just $ Leaf (V2 10 10) 7
                    , quadChildren =
                        ( Nothing
                        , Just $
                          LeafNode (Rect (V2 0 25) 25 25) (Leaf (V2 10 30) 12)
                        , Just $
                          LeafNode (Rect (V2 0 0) 25 25) (Leaf (V2 10 10) 7)
                        , Nothing)
                    }
                , Nothing)
            }
    it "finds nearest node" $ do
      nearest (V2 10 12) treeWithTwoLeaves `shouldBe` (Just $ Leaf (V2 10 10) 7)
