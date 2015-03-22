import qualified Data.Array as A

main :: IO ()
main = getContents >>= mapM_ print . solve . map (map read . words) . lines

solve :: [[Int]] -> [Int]
solve [[n], as, _, qs] = map (binsearch (A.listArray (0, n) (scanl (+) 0 as))) qs
solve _ = undefined

binsearch :: A.Array Int Int -> Int -> Int
binsearch xs x = go (A.bounds xs)
  where go (a, b) | a + 1 == b = b
                  | x <= xs A.! c = go (a, c)
                  | otherwise = go (c, b)
          where c = (a + b) `div` 2
