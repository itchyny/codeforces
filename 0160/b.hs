import Control.Applicative ((<$>), (<*>))
import Data.Char (digitToInt)
import Data.List (sort)

main :: IO ()
main = solve <$> (splitAt <$> readLn <*> (map digitToInt <$> getLine)) >>= putStrLn . yesno

solve :: ([Int], [Int]) -> Bool
solve (xs, ys) = and (zipWith (>) (sort xs) (sort ys)) || and (zipWith (<) (sort xs) (sort ys))

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
