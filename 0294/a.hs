import Control.Applicative ((*>), (<$>), (<*>))

main :: IO ()
main = getLine >> solve <$> (map read . words <$> getLine) <*> (getLine *> (map (map read . words) . lines <$> getContents)) >>= mapM_ print

solve :: [Int] -> [[Int]] -> [Int]
solve = foldl (\as [x, y] -> [ sum $ [ a | i /= x ] ++ [ y - 1 | i == x - 1 ] ++ [ as !! (x - 1) - y | i == x + 1 ] | (i, a) <- zip [1..] as ])
