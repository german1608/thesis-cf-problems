{-| Version that is more similar to how MagicCP actually process the input and output
-}
import Data.Maybe
import Data.Graph
import Data.Tree
import Text.Read
import Control.Monad

root = -1

indexAsVertex :: [Int] -> [(Int, Int)]
indexAsVertex = flip zip [1..]

longestPathOnTree :: Graph -> Int
longestPathOnTree g = length (levels $ head $ dfs g [root]) - 1


parseGraphFromSizeAndIndexToVertexOnSepLines :: String -> Maybe Graph
parseGraphFromSizeAndIndexToVertexOnSepLines i = do
    let ls = lines i
    when (null ls) Nothing
    let l1:l2 = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe l2 :: Maybe [Int]
    let edges = zip as [1..]
    return $ buildG (foldl min n as, n) edges

parse = fromJust . parseGraphFromSizeAndIndexToVertexOnSepLines
solve = longestPathOnTree

main = getContents >>= (print . solve . parse)
