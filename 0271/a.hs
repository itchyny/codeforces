import Data.List (nub)

main :: IO ()
main = readLn >>= print . solve

solve :: Int -> Int
solve = head . filter isDistinct . enumFrom . succ

isDistinct :: Int -> Bool
isDistinct n = length (show n) == length (nub (show n))
