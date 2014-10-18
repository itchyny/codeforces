import Data.List (sort)

main :: IO ()
main = getContents >>= putStrLn . yesno . solve . lines

solve :: [String] -> Bool
solve xs = sort (last xs) == sort (concat (init xs))

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
