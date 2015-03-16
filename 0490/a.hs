import Data.List (elemIndex, unfoldr)

main :: IO ()
main = getContents >>= mapM_ (putStrLn . unwords . map show) . solve . map read . words . (!!1) . lines

solve :: [Int] -> [[Int]]
solve ts = [length xs] : xs
  where xs = unfoldr f ts
        f as = case (elemIndex 1 as, elemIndex 2 as, elemIndex 3 as) of
                    (Just i1, Just i2, Just i3) -> Just ([ i1 + 1, i2 + 1, i3 + 1 ], zipWith g [0..] as)
                      where g i a | i `elem` [ i1, i2, i3 ] = 0 | otherwise = a
                    _ -> Nothing
