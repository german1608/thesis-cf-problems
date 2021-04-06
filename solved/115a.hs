import Control.Monad
import Data.Graph
import Data.Tree

root = -1

indexAsVertex :: [Int] -> [(Int, Int)]
indexAsVertex = flip zip [1..]

longestPathOnTree :: Graph -> Int
longestPathOnTree g = length (levels $ head $ dfs g [root]) - 1

main = do
    n <- (read :: String -> Int) <$> getLine
    arr <- replicateM n ((read :: String -> Int) <$> getLine)
    let edges = indexAsVertex arr
    -- print edges
    -- print arr
    let g = buildG (root, n) edges
    -- print g
    print $ longestPathOnTree g
