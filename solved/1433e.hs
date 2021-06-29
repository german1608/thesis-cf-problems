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

parseInt :: String -> Maybe Int
parseInt (lines -> [l1]) = Text.Read.readMaybe l1 :: Maybe Int
parseInt _ = Nothing

factorial _ 0 = 1
factorial n i = n * factorial (n-1) (i-1)

solve :: Int -> String
solve (toInteger -> i) = show $ truncate $ fromIntegral (factorial i i) / fromIntegral (2 * (i2 ^ 2))
  where
    i2 = truncate (fromIntegral i/2)


parse = fromJust . parseInt

main = getContents >>= (putStrLn . solve . parse)
