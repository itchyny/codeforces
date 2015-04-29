import Data.List (transpose)

main :: IO ()
main = getContents >>= print . solve . tail . lines

solve :: [String] -> Int
solve xss = length [ () | xs <- xss, any (uncurry (==)) $ zip xs (map maximum $ transpose xss) ]
