import Data.Maybe
import Data.Graph
import Data.Tree
import Data.Char
import Text.Read
import qualified Data.Map as Map
import Control.Monad
import Debug.Trace (traceShow)

type StringGraph = (Graph, Vertex -> (String, String, [String]), String -> Vertex)

parseGraphFromSizeIgnoringItAndExplicitEdgesCaseInsensitive :: String -> Maybe StringGraph
parseGraphFromSizeIgnoringItAndExplicitEdgesCaseInsensitive i = do
    let ls = lines $ map toLower i
    when (length ls < 2) Nothing

    let _ : inputEdges = ls
    let mkEdge s = do
            let ws = words s
            when (length ws < 2) Nothing
            return [Map.singleton (head ws) [], Map.singleton (last ws) [head ws]]

    edges <- map (\(f, s) -> (f, f, s)) . Map.toList . Map.unionsWith (<>) . concat <$> mapM mkEdge inputEdges

    let (g, v, s) = graphFromEdges  edges
    return (g, v, fromJust . s)

longestPathOnTree :: StringGraph -> Int
longestPathOnTree (g, _, vf) = length (levels $ head $ dfs g [vf "polycarp"])

parse = fromJust . parseGraphFromSizeIgnoringItAndExplicitEdgesCaseInsensitive

solve :: StringGraph -> String
solve = show . longestPathOnTree

main = getContents >>= (putStrLn . solve . parse)
