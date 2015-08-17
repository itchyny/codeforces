import Data.List (sort)

main :: IO ()
main = putStrLn . unwords . map show . solve . tail . map read . words =<< getContents

solve :: [Int] -> [Int]
solve xs = [ maximum xs ] ++ init (tail (sort xs)) ++ [ minimum xs ]
