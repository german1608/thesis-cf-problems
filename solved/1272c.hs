{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.Graph
import Data.Tree
import qualified Data.List as List
import qualified Text.Read
import qualified Data.Map
import Data.Map (Map)
import Control.Monad

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy _ [] = []
wordsBy f as =
    let (include, remaining) = List.break f as
        (remove, remaining') = List.span f remaining
    in  include : wordsBy f remaining'

parsePairStringWithSizes :: String -> Maybe (String, String)
parsePairStringWithSizes (lines -> [_, l2, l3]) =
    Just (l2, filter (/= ' ') l3)
parsePairStringWithSizes _ = Nothing

solve :: (String, String) -> String
solve (s, ks) = show $ truncate $ sum $ map substringCount $ wordsBy (not . flip elem ks) s
  where
    substringCount s =
        let n = fromIntegral (length s) in n * (n+1) / 2



parse = fromJust . parsePairStringWithSizes

main = getContents >>= (putStrLn . solve . parse)
