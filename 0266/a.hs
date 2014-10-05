import Data.List (group)

main :: IO ()
main = getContents >>= print . solve . (!!1) . lines

solve :: String -> Int
solve cs = length cs - length (group cs)
