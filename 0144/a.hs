import Data.List ((\\), elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = getContents >>= print . solve . map read . words . (!!1) . lines

solve :: [Int] -> Int
solve xs = fromJust (elemIndex (maximum xs) xs) + fromJust (elemIndex (minimum xs) xs')
  where xs' = reverse (xs \\ [maximum xs])
