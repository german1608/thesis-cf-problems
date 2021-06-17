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

parseTwoInts :: String -> Maybe (Int, Int)
parseTwoInts (words -> [ns, ms]) =
    Applicative.liftA2 (,) (Text.Read.readMaybe ns :: Maybe Int) (Text.Read.readMaybe ms :: Maybe Int)
parseTwoInts _ = Nothing

comb :: Integer -> Integer
comb (fromInteger -> n) = truncate (n * (n - 1) / 2)

solve :: (Int, Int) -> String
solve (toInteger -> n, toInteger -> m) = show mi <> " " <> show ma
  where
    f x = if x < 2 then 1 else comb x
    mi = f $ floor (fromInteger n / fromInteger m)
    ma = f (n - m + 1)




parse = fromJust . parseTwoInts

main = getContents >>= (putStrLn . solve . parse)
