import qualified Data.Map as M

main :: IO ()
main = getContents >>= putStrLn . unwords . solve . map words . tail . lines

solve :: [[String]] -> [String]
solve cs = map (\s -> M.findWithDefault s s m) $ last cs
  where m = M.fromList $ filter (\(x, y) -> length x > length y) $ map toTuple $ init cs

toTuple :: [a] -> (a, a)
toTuple xs = (head xs, xs !! 1)
