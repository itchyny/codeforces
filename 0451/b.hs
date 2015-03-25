import Data.List (sort)

main :: IO ()
main = getContents >>= putStrLn . solve . map read . tail . words

solve :: [Int] -> String
solve as | null is = unlines [ "yes", "1 1" ]
         | reverseSegment imin imax as == sort as = unlines [ "yes", unwords (map show [ imin, imax ]) ]
         | otherwise = "no"
  where is = [ i | (i, a, a') <- zip3 [1..] as (sort as), a /= a' ]
        imin = head is; imax = last is

reverseSegment :: Int -> Int -> [a] -> [a]
reverseSegment i j as = take (i - 1) as ++ reverse (drop (i - 1) (take j as)) ++ drop j as
