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
solve n = show $ 2 ^ (n + 1) - 2



parse = fromJust . parseSingleInt

main = getContents >>= (putStrLn . solve . parse)
