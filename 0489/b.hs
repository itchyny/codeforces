import Data.List (sort)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Integer]] -> Integer
solve [_, as, _, bs] = go (sort as) (sort bs)
  where go xxs@(x:xs) yys@(y:ys) | x + 1 < y = go xs yys
                                 | y + 1 < x = go xxs ys
                                 | otherwise = 1 + go xs ys
        go _ _ = 0
solve _ = undefined
