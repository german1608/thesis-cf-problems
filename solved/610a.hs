{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.Graph
import Data.Tree
import qualified Text.Read
import qualified Data.Map
import Data.Map (Map)
import Control.Monad

parseSingleInt :: String -> Maybe Int
parseSingleInt s = Text.Read.readMaybe s :: Maybe Int

solve :: Int -> String
solve n = show $ if odd n then 0 else ceiling (fromIntegral n / 4) - 1



parse = fromJust . parseSingleInt

main = getContents >>= (putStrLn . solve . parse)
