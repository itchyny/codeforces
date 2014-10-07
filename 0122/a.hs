main :: IO ()
main = readLn >>= putStrLn . solve

solve :: Int -> String
solve = yesno . any isLucky . factors

isLucky :: Int -> Bool
isLucky = all (`elem`"47") . show

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"

factors :: Integral t => t -> [t]
factors n = [m | m <- [1..n], n `mod` m == 0]
