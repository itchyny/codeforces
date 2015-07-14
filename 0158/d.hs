import Control.Applicative ((<$>), (<*>))
import Data.List (transpose, unfoldr)

main :: IO ()
main = solve <$> readLn <*> (map read . words <$> getLine) >>= print

solve :: Int -> [Int] -> Int
solve n ts = maximum [ maximum $ map sum $ transpose $ splitN (n `div` m) ts | m <- [3..n], n `mod` m == 0 ]
                                                                                                            
splitN :: Int -> [a] -> [[a]]
splitN n = takeWhile (not . null) . unfoldr (Just . splitAt n)
