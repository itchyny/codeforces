import Data.Char (digitToInt)
import Data.Functor ((<$>))

main :: IO ()
main = getLine >>= mapM_ (putStrLn . solve . digitToInt) . reverse

solve :: Int -> String
solve n = maybe '|' (("-O"!!) . fromEnum) <$> Just (n < 5) : Just (5 <= n) : Nothing : [ Just (n `mod` 5 /= i) | i <- [0..4] ]
