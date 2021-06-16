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
solve n = show $ sum $ map ((1 /). fromIntegral) [1..n]



parse = fromJust . parseSingleInt

main = getContents >>= (putStrLn . solve . parse)
