import Data.List ((\\), sort)

main :: IO ()
main = getContents >>= mapM_ (\xs -> putStrLn $ unwords $ map show $ length xs : xs) . solve . map read . tail . words

solve :: [Int] -> [[Int]]
solve as = [ n1, n2, (as \\ n1) \\ n2 ]
  where n1 = [ minimum as ]
        n2 = if maximum as > 0 then [ maximum as ] else take 2 (tail (sort as))
