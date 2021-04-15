{-# LANGUAGE ViewPatterns #-}
import Data.Graph (Graph, Table)
import qualified Data.Array as Array
import qualified Data.Maybe as Maybe
import qualified Data.Graph as Graph
import qualified Text.Read
import qualified Data.Map as Map

{-|
   n m
   v_1 w_1
   v_2 w_2
   ...
   v_m w_m
-}
parseUndirectedGraphFromVertexAndEdgesCountAndEdgesPairOnSepLines :: String -> Maybe Graph
parseUndirectedGraphFromVertexAndEdgesCountAndEdgesPairOnSepLines (lines -> ls)
    | Just (sizes : edges) <- mapM (\l -> mapM Text.Read.readMaybe (words l) :: Maybe [Int]) ls :: Maybe [[Int]]
    , [numV, numE] <- sizes
    , Just edges' <- mapM listToPair edges
    , undirEdges <- concatMap (\(i, j) -> if i /= j then [(i, j), (j, i)] else [(i, j)]) edges'
    = Just $ Graph.buildG (1, numV) undirEdges
  where
    listToPair [i, j] = Just (i, j)
    listToPair _ = Nothing
parseUndirectedGraphFromVertexAndEdgesCountAndEdgesPairOnSepLines _ = Nothing

parse = Maybe.fromJust . parseUndirectedGraphFromVertexAndEdgesCountAndEdgesPairOnSepLines

busGraph :: Table Int -> Bool
busGraph outdegreeT = twoOnes && restTwos
  where
    values = Array.elems outdegreeT
    numValues = length values
    twoOnes = 2 == length (filter (==1) values)
    restTwos = (numValues - 2) == length (filter (==2) values)


cycleGraph :: Table Int -> Bool
cycleGraph outdegreeT = allTwos
  where
    values = Array.elems outdegreeT
    allTwos = all (==2) values


starGraph :: Table Int -> Bool
starGraph outdegreeT = onlyOneCenter && restOnes
  where
    values = Array.elems outdegreeT
    numValues = length values
    onlyOneCenter = 1 == length (filter (==(numValues - 1)) values)
    restOnes = (numValues - 1) == length (filter (==1) values)

solve :: Graph -> String
solve (Graph.outdegree -> o)
    | busGraph o = "bus topology"
    | cycleGraph o = "ring topology"
    | starGraph o = "star topology"
    | otherwise = "unknown topology"

main = getContents >>= (putStrLn . solve . parse)
