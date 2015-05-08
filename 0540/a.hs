import Control.Applicative ((<$>), (<*>))
import Data.Char (digitToInt)

main :: IO ()
main = getLine >> solve <$> (map digitToInt <$> getLine) <*> (map digitToInt <$> getLine) >>= print

solve :: [Int] -> [Int] -> Int
solve xs ys = sum $ zipWith (\x y -> 5 - abs (abs (x - y) - 5)) xs ys
