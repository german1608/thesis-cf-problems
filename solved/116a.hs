{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.Graph
import Data.Tree
import qualified Text.Read
import qualified Data.Map
import Data.Map (Map)
import Control.Monad

{-
n
word_1
word_2
...
word_n

<=>

[
    word_1,
    word_2,
    ...,
    word_n
]
-}
parseListOfString :: String -> Maybe [String]
parseListOfString (lines -> ts : ls) = Just ls

solve :: [String] -> String
solve names = show ans
  where
    firstLetters = map head names

    charOccurs = Data.Map.elems
        $ Data.Map.unionsWith (+)
        $ map (`Data.Map.singleton` 1) firstLetters

    comb :: Integer -> Integer
    comb (fromInteger -> n) = truncate (n * (n - 1) / 2)

    split :: Integer -> [Integer]
    split cc =
        let half = fromInteger cc / 2
        in  [floor half, ceiling half]

    ans = sum $ map comb $ concatMap split charOccurs



parse = fromJust . parseListOfString

main = getContents >>= (putStrLn . solve . parse)
