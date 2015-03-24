import Data.Char (digitToInt)
import Data.List (sortBy)

main :: IO ()
main = getLine >> getLine >>= putStrLn . concatMap show . solve

solve :: String -> [Int]
solve = sortBy (flip compare) . concatMap (f . digitToInt)
   where f 0 = []
         f 1 = []
         f 4 = [2, 2, 3]
         f 6 = [3, 5]
         f 8 = [2, 2, 2, 7]
         f 9 = [2, 3, 3, 7]
         f x = [x]
