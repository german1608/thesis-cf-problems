import Data.Maybe
import Data.Graph
import Data.Tree
import Text.Read
import Control.Monad

root = -1

indexAsVertex :: [Int] -> [(Int, Int)]
indexAsVertex = flip zip [1..]


hasCycleWithLength :: Int -> Graph -> Bool
hasCycleWithLength n = any ((==n) . length . flatten) . scc
longestPathOnTree :: Graph -> Int
longestPathOnTree g = length (levels $ head $ dfs g [root]) - 1

parseGraphFromSizeAndIndexToVertexOnTwoLines :: String -> Maybe Graph
parseGraphFromSizeAndIndexToVertexOnTwoLines i = do
    let ls = lines i
    when (null ls) Nothing
    let [l1, l2] = ls
    n <- readMaybe l1 :: Maybe Int
    as <- mapM readMaybe (words l2) :: Maybe [Int]
    let edges = zip [1..] as
    return $ buildG (1, n) edges

yesNo :: Bool -> String
yesNo True = "YES"
yesNo False = "NO"

parse = fromJust . parseGraphFromSizeAndIndexToVertexOnTwoLines
solve = yesNo . hasCycleWithLength 3

main = getContents >>= (putStrLn . solve . parse)
