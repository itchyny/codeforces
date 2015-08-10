import Control.Applicative ((<$>), (<*>))
import Data.Graph (buildG, components)
import Data.List ((\\))
import Data.Tree (flatten)

main :: IO ()
main = print =<< solve <$> readTuple <*> readLists

solve :: (Int, Int) -> [[Int]] -> Int
solve (n, m) xs | all null xs = n
                | otherwise = length (map flatten (components graph) \\ map (:[]) [n + 1 .. n + m]) - 1
  where graph = buildG (1, n + m) $ concatMap (\(i, ys) -> map (\y -> (i, n + y)) ys) $ zip [1..] xs

toTuple :: [a] -> (a, a)
toTuple xs = (head xs, xs !! 1)

readTuple :: Read a => IO (a, a)
readTuple = toTuple . map read . words <$> getLine

readLists :: Read a => IO [[a]]
readLists = map (map read . tail . words) . lines <$> getContents
