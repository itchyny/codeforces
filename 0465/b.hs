import Data.List (group)

main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Int] -> Int
solve = max 0 . pred . sum . map (succ . length) . filter ((>0) . head) . group
