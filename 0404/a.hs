main :: IO ()
main = getContents >>= putStrLn . yesno . solve . tail . lines

solve :: [String] -> Bool
solve css@((x:y:_):_) = x /= y && and [ and [ c == f i j | (j, c) <- zip [0..] cs ] | (i, cs) <- zip [0..] css ]
  where f i j = if i == j || i + j + 1 == length css then x else y
solve _ = undefined

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
