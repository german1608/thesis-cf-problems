import Control.Monad

lowerBound :: Int -> Int -> Int -> (Int -> Int) -> Int
lowerBound lo hi target f
  | 
  where
    mid = case (lo + hi)
    lof = f lo
    hif = f hi
    tf = f target

main = do
    n <- (read :: String -> Int) <$> getLine
    arr <- replicateM n $ ((read :: String -> Int) <$> getLine)
    print n
    print arr
