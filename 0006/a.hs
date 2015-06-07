import Data.Functor ((<$>))
import Data.List (sort)

main :: IO ()
main = putStrLn =<< solve <$> sort <$> map read <$> words <$> getLine

solve :: [Integer] -> String
solve [ a, b, c, d ] | a + b > c || b + c > d = "TRIANGLE"
                     | a + b == c || b + c == d = "SEGMENT"
                     | otherwise = "IMPOSSIBLE"
solve _ = undefined
