import Data.List (unfoldr)

main :: IO ()
main = readLn >>= mapM_ (putStrLn . unwords . map show) . solve

solve :: Int -> [[Int]]
solve n = map concat $ splitN (n `div` 2) $ map (\x -> [ x, n * n + 1 - x ]) [1 .. n * n `div` 2]

splitN :: Int -> [a] -> [[a]]
splitN n = takeWhile (not . null) . unfoldr (Just . splitAt n)
