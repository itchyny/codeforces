import Control.Applicative ((<$>), (<*>))
import Data.Array ((!), listArray)
import Data.List (sortBy)
import Data.Maybe (listToMaybe)

main :: IO ()
main = print =<< solve <$> readLn <*> (map read . words <$> getLine)

solve :: Int -> [Int] -> Int
solve n as = length [ a | (i, a) <- zip [0..] as, i == a ]
           + maybe 0 succ (listToMaybe (sortBy (flip compare) [ fromEnum (as' ! a == i) | (i, a) <- zip [0..] as, i /= a ]))
  where as' = listArray (0, n - 1) as
