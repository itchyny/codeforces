import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = getContents >>= mapM_ putStrLn . solve . map words . tail . lines

solve :: [[String]] -> [String]
solve = map head . sortBy (compare `on` priority . (!!1))
  where priority "rat" = 0 :: Int
        priority "woman" = 1
        priority "child" = 1
        priority "man" = 2
        priority "captain" = 3
        priority _ = 4
