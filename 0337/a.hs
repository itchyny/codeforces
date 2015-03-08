import Data.List

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[n, _], fs] = minimum [ a - b | (a, b) <- zip (drop (n - 1) fs') fs' ]
  where fs' = sort fs
solve _ = undefined
