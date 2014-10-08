import Data.List (elemIndex, foldl', unfoldr)
import Data.Maybe (fromJust)

main :: IO ()
main = getContents >>= putStrLn . solve . map read . words . (!!1) . lines

solve :: [Int] -> String
solve = (exercise !!) . maximumIndex . foldl' (zipWith (+)) [0, 0, 0] . map (++[0,0,0]) . splitN 3

exercise :: [String]
exercise = [ "chest", "biceps", "back" ]

splitN :: Int -> [a] -> [[a]]
splitN n = takeWhile (not . null) . unfoldr (Just . splitAt n)

maximumIndex :: Ord a => [a] -> Int
maximumIndex xs = fromJust $ elemIndex (maximum xs) xs
