module Connect (Mark(..), winner) where

import Data.Function                         (on)
import Data.Graph.Inductive                  (Gr, mkGraph)
import Data.Graph.Inductive.Query.DFS        (components)
import Data.List                             (elemIndex)
import Data.Maybe                            (fromJust)
import Data.Set                              (fromList, isSubsetOf)
import Math.Geometry.Grid                    (neighbours)
import Math.Geometry.Grid.HexagonalInternal  (UnboundedHexGrid(..))


data Mark = Cross | Nought deriving (Eq, Show)

type Point = (Int,Int)

winner :: [String] -> Maybe Mark
winner xss
    | any (fst `hasValues` [1,height]) (connectedGraphsOf 'O') = Just Nought
    | any (snd `hasValues` [1,width ]) (connectedGraphsOf 'X') = Just Cross
    | otherwise                                                = Nothing
  where
    height = length xss
    width  = maximum $ length <$> xss
    connectedGraphsOf x = connectedHexGraphs $ indicesOf x xss
    f `hasValues` xs = (isSubsetOf `on` fromList) xs . map f

indicesOf :: Eq a => a -> [[a]] -> [Point]
indicesOf e xss = [ (i, j) | (i, xs) <- zip [1..] xss
                           , (j, x ) <- zip [1..] xs
                           ,  x == e ]

connectedHexGraphs :: [Point] -> [[Point]]
connectedHexGraphs nodes = map (map fromNode) $ components hexGraph
  where
    lNodes   = zip [0..] nodes
    toNode x = fromJust $ elemIndex x nodes
    fromNode = (nodes !!)
    lEdges = [(toNode p, toNode q, ()) | (p, q) <- hexEdges nodes]
    hexGraph = mkGraph lNodes lEdges :: Gr Point ()

hexEdges :: [Point] -> [(Point, Point)]
hexEdges ps = [(p,q) | p <- ps, q <- ascendingEdges p]
  where
    ascendingEdges p = [n | n <- neighbours UnboundedHexGrid p
                          , n > p
                          , n `elem` ps ]

