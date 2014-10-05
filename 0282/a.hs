import Data.List (foldl')

main :: IO ()
main = getContents >>= print . solve . tail . lines

solve :: [String] -> Int
solve = foldl' f 0
  where f n "++X" = n + 1
        f n "X++" = n + 1
        f n "--X" = n - 1
        f n "X--" = n - 1
        f n _ = n
