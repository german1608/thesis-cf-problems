{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.Graph
import Data.Tree
import qualified Data.List as List
import qualified Text.Read
import qualified Data.Map
import Data.Map (Map)
import Control.Monad

parsePairStringWithSizes :: String -> Maybe [Int]
parsePairStringWithSizes (lines -> [_, words -> items]) =
    mapM (\s -> Text.Read.readMaybe s :: Maybe Int) items
parsePairStringWithSizes _ = Nothing

solve :: [Int] -> String
solve xs = show (ma - mi) <> " " <> show (
    if ma /= mi
    then num_mi * num_ma
    else truncate (fromIntegral ((num_mi - 1) * num_mi) / 2))
  where
    mi = foldl min (head xs) xs
    ma = foldl max 0 xs

    occurr i acc a = acc + if i == a then 1 else 0

    num_mi :: Integer
    num_mi = foldl (occurr mi) 0 xs
    num_ma :: Integer
    num_ma = foldl (occurr ma) 0 xs




parse = fromJust . parsePairStringWithSizes

main = getContents >>= (putStrLn . solve . parse)
