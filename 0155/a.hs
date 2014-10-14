import Data.List (inits)

main :: IO ()
main = getContents >>= print . solve . map read . words . (!!1) . lines

solve :: [Int] -> Int
solve xs = length $ filter id $ zipWith f (tail xs) (tail $ inits xs)
  where f y ys = y < minimum ys || maximum ys < y
