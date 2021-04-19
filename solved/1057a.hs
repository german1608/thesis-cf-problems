{-# LANGUAGE ViewPatterns #-}
import Data.Graph (Graph, Table, Vertex)
import Data.Tree (Tree (..))
import qualified Data.Array as Array
import qualified Data.Maybe as Maybe
import qualified Data.Graph as Graph
import qualified Text.Read
import qualified Data.Map as Map
import qualified Control.Monad

{-| Parser for the following input:

    n
    p_2 p_3 ... p_n

where $$i$$ has a directed edge to $$p_i$$. Usually used for trees.
-}
parseDirectedGraphFromSizeAndSingleLineOfSizeMinusOne :: String -> Maybe Graph
parseDirectedGraphFromSizeAndSingleLineOfSizeMinusOne (lines -> ls)
    | Just [[numV], edges] <- mapM (\l -> mapM Text.Read.readMaybe (words l) :: Maybe [Int]) ls :: Maybe [[Int]]
    , length edges == numV - 1
    , edges' <- zip edges [2..]
    = Just $ Graph.buildG (1, numV) edges'
  where
    listToPair [i, j] = Just (i, j)
    listToPair _ = Nothing
parseDirectedGraphFromSizeAndSingleLineOfSizeMinusOne _ = Nothing

parse = Maybe.fromJust . parseDirectedGraphFromSizeAndSingleLineOfSizeMinusOne

pathBetween :: Graph -> Vertex -> Vertex -> Maybe [Vertex]
pathBetween g v w = go spanningTree []
  where
    spanningTree = head $ Graph.dfs g [v]

    go :: Tree Vertex -> [Vertex] -> Maybe [Vertex]
    go (Node i ts) currentPath
      | i == w = Just (reverse (w : currentPath))
      | otherwise = case Maybe.mapMaybe (`go` (i:currentPath)) ts of
          [] -> Nothing
          x:_ -> Just x


solve :: Graph -> String
solve g = prettyPrint path
  where
    path = case pathBetween g 1 (snd $ Array.bounds g) of
      Nothing -> error "No path found"
      Just x -> x
    prettyPrint = unwords . map show

main = getContents >>= (putStrLn . solve . parse)
