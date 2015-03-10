import Data.List (nub)

main :: IO ()
main = getContents >>= putStrLn . output . solve . map (map read . words) . lines

solve :: [[Int]] -> Bool
solve [[n], _:xs, _:ys] = length (nub (xs ++ ys)) == n
solve _ = undefined

output :: Bool -> String
output True = "I become the guy."
output _ = "Oh, my keyboard!"
