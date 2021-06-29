{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.Graph
import Data.Tree
import qualified Data.List as List
import qualified Text.Read
import qualified Data.Map
import qualified Control.Applicative as Applicative
import Data.Map (Map)
import Control.Monad
import Data.Array (Array, (!))
import qualified Numeric
import qualified Debug.Trace

parseTwoStringLines :: String -> Maybe (String, String)
parseTwoStringLines (lines -> [l1, l2]) = Just (l1, l2)
parseTwoStringLines _ = Nothing

pos :: String -> Integer
pos = sum . map (\b -> if b == '+' then 1 else -1)

gen :: String -> [String]
gen s = if numOfHoles == 0 then [s] else gens
  where
    numOfHoles = length $ filter (=='?') s

    f i = let s = Numeric.showIntAtBase 2 (\s -> if s == 0 then '-' else '+') i ""
          in  fixLength (length s) s

    masks = map f [0..((2^numOfHoles) - 1)]

    fixLength i s
        | i == numOfHoles = s
        | otherwise = fixLength (i+1) ('-' : s)

    gens = map (removeHole s) masks

    removeHole [] _ = []
    removeHole ('?' : path) (m : mask) = m : removeHole path mask
    removeHole (p : path) mask = p : removeHole path mask

solve :: (String, String) -> String
solve (l1, l2) = show $ fromIntegral correctPosibilities / fromIntegral (length posibilities)
  where
    specPos = pos l1
    posibilities = gen l2
    correctPosibilities = length $ filter (==specPos) $ map pos posibilities




parse = fromJust . parseTwoStringLines

main = getContents >>= (putStrLn . solve . parse)
