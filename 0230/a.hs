import Data.List (sort)

main :: IO ()
main = getContents >>= putStrLn . yesno . solve . map (map read . words) . lines

solve :: [[Int]] -> Bool
solve ([s, _]:xys) = and $ zipWith (<) (map head xys') (scanl (+) s (map (!!1) xys'))
  where xys' = sort xys
solve _ = undefined

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
