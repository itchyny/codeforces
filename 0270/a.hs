main :: IO ()
main = getContents >>= mapM_ (putStrLn . yesno . solve . read) . tail . lines

solve :: Int -> Bool
solve a = 360 `mod` (180 - a) == 0

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
