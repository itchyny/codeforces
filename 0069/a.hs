import Data.List (foldl')

main :: IO ()
main = getContents >>= putStrLn . yesno . solve . map (map read . words) . tail . lines

solve :: [[Int]] -> Bool
solve = all (==0) . foldl' (zipWith (+)) [0, 0, 0]

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
