import Data.Array.Unboxed ((!), Array, listArray)

main :: IO ()
main = getLine >>= \s -> getContents >>= mapM_ print . solve s . map (map read . words) . tail . lines

solve :: String -> [[Int]] -> [Int]
solve s lrs = [ dp ! r - dp ! l | [ l, r ] <- lrs ]
  where dp :: Array Int Int
        dp = listArray (1, length s) $ scanl (+) 0 $ zipWith (\x y -> fromEnum (x == y)) s $ tail s
