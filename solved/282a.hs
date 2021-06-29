{-# LANGUAGE ViewPatterns #-}
import qualified Data.Maybe
import qualified Data.List as List
import qualified Text.Read
import qualified Data.Map
import qualified Control.Applicative as Applicative
import qualified Numeric
import qualified Debug.Trace

parseListOfStringsWithoutSize :: String -> Maybe [String]
parseListOfStringsWithoutSize (lines -> _ : s) = Just s
parseListOfStringsWithoutSize _ = Nothing


solve :: [String] -> String
solve = show . foldl (\acc op -> acc + if op `elem` ["--X", "X--"] then -1 else 1) 0


parse = Data.Maybe.fromJust . parseListOfStringsWithoutSize

main = getContents >>= (putStrLn . solve . parse)
