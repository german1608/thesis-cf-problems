{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.Graph
import Data.Tree
import qualified Text.Read
import Control.Monad

{-
t
n_1 k1_1 k2_1
w_1 b_1
n_2 k1_2 k2_2
w_2 b_2
...
n_t k1_t k2_t
w_t b_t

<=>

[
    (n_1, k1_1, k2_1, w_1, b_1),
    (n_2, k1_2, k2_2, w_2, b_2),
    ...
    (n_t, k1_t, k2_t, w_t, b_t),
]
-}
parseTestCases :: String -> Maybe [(Int, Int, Int, Int, Int)]
parseTestCases (lines -> ts : ls)
    | Just t <- Text.Read.readMaybe ts :: Maybe Int
    = mapM (uncurry parseTwoLines) $ group2 ls
  where
    group2 :: [a] -> [(a, a)]
    group2 [] = []
    group2 [a] = error "Single element is not allowed"
    group2 (x:y:as) = (x, y) : group2 as


    parseTwoLines :: String -> String -> Maybe (Int, Int, Int, Int, Int)
    parseTwoLines (words -> [n, k1, k2]) (words -> [w, b])
        | Just n' <- Text.Read.readMaybe n :: Maybe Int
        , Just k1' <- Text.Read.readMaybe k1 :: Maybe Int
        , Just k2' <- Text.Read.readMaybe k2 :: Maybe Int
        , Just w' <- Text.Read.readMaybe w :: Maybe Int
        , Just b' <- Text.Read.readMaybe b :: Maybe Int
        = Just (n', k1', k2', w', b')
    parseTestCases _ _ = Nothing

solveTestCases :: [(Int, Int, Int, Int, Int)] -> String
solveTestCases = init . unlines . map solve

solve :: (Int, Int, Int, Int, Int) -> String
solve (n, k1, k2, w, b) =
    if yesw && yesb  then "YES"
    else "NO"
  where
    l = k1 `min` k2
    r = k1 `max` k2
    ib = floor (abs (fromInteger (toInteger (k1 - k2))) / 2)

    rw = w - min w l
    rb = b - (min b r + 1)

    yesw = rw <= ib
    yesb = rb <= ib

parse = fromJust . parseTestCases

main = getContents >>= (putStrLn . solveTestCases . parse)
