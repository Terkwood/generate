module Generate.Patterns.Tangles (tangles) where

import Linear
import Data.Maybe
import qualified Data.Vector as V
import qualified Streaming.Prelude as S

import Generate.Monad
import Generate.Stream
import Generate.Geom (toVertices)
import Generate.Geom.Line
import Generate.Geom.Rect
import qualified Generate.Algo.QuadTree as Q

data TangleSearch = TangleSearch { minLen :: Int, idx :: Int, points :: Stream (V2 Double), tree :: Q.QuadTree Int, currentPath :: [V2 Double], tolerance :: Double }

tangles :: Double -> Int -> Stream (V2 Double) -> Stream Line
tangles tolerance minLen points = S.mapMaybe fst $ S.takeWhile (isJust . snd) $ S.iterateM searchStep $ do
    region <- fullFrame
    return $ (Nothing, Just $ TangleSearch minLen 0 points (Q.new region) [] tolerance)

searchStep :: (Maybe Line, Maybe TangleSearch) -> Generate (Maybe Line, Maybe TangleSearch)
searchStep (_, Nothing) = return (Nothing, Nothing)
searchStep (_, Just s) = tangleSearch s

tangleSearch :: TangleSearch -> Generate (Maybe Line, Maybe TangleSearch)
tangleSearch search@(TangleSearch {..}) = do
    nextStep <- S.next points
    region <- fullFrame
    case nextStep of
        Left _ -> return $ (Nothing, Nothing)
        Right (position, rest) -> do
            let leaf = Q.Leaf position idx
            let (_, tree') = Q.insert tree leaf
            let path' = position : currentPath
            let search' = search { idx = idx + 1, points = rest, tree = tree', currentPath = path' }
            return $ case Q.nearest tree leaf of
                Just (Q.Leaf np j) -> if distance np position >= tolerance || idx - j < minLen
                    then (Nothing, Just search' )
                    else (mkLine $ V.fromList $ take (idx - j + 1) path', Just $ search' { idx = 0, tree = Q.new region })
                Nothing -> (Nothing, Just search')