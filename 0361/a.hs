import Data.Functor ((<$>))
import Data.List (unfoldr)

main :: IO ()
main = mapM_ (putStrLn . unwords . map show) =<< solve . toTuple . map read . words <$> getLine

solve :: (Int, Int) -> [[Int]]
solve (n, k) = take n $ splitN n $ cycle $ k : replicate n 0

splitN :: Int -> [a] -> [[a]]
splitN n = takeWhile (not . null) . unfoldr (Just . splitAt n)

toTuple :: [a] -> (a, a)
toTuple xs = (head xs, xs !! 1)
