import Data.Maybe
import Data.Graph
import Data.Tree
import Text.Read
import Control.Monad

parseGraphFromSizeAndIndexToVertexOnTwoLines :: String -> Maybe Graph
parseGraphFromSizeAndIndexToVertexOnTwoLines i = do
    let ls = lines i
    when (null ls) Nothing
    let [l1, l2] = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe (words l2) :: Maybe [Int]
    let edges = zip [1..] as
    return $ buildG (1, n) edges

dfsForEachVertex :: Show a => Graph -> (Tree Vertex -> a) -> [a]
dfsForEachVertex g f =
    map f fs
  where
    vs = vertices g
    fs = map (head . dfs g . (:[])) vs

joinWithSpaces :: Show a => [a] -> String
joinWithSpaces = unwords . map show

-- | Assumes that the outdegree of every vertex is 1
getSingleEnd :: Graph -> Vertex -> Int
getSingleEnd graph vertex =
    snd $ head $ filter ((==vertex) . fst) $ edges graph

getSingleLeaf :: Tree Vertex -> Vertex
getSingleLeaf = last . flatten

parse = fromJust . parseGraphFromSizeAndIndexToVertexOnTwoLines

solve :: Graph -> String
solve g = joinWithSpaces $ dfsForEachVertex g (getSingleEnd g . getSingleLeaf)

main = getContents >>= (putStrLn . solve . parse)
