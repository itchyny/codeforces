import Data.List (group)

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (_:t:c:xs) = sum $ filter (>0) $ map (subtract (c - 1) . length) $ filter head $ group $ map (<=t) xs
solve _ = undefined
