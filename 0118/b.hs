main :: IO ()
main = readLn >>= mapM_ putStrLn . solve

solve :: Int -> [String]
solve n = [ replicate (2 * (n - i)) ' ' ++ unwords (map show $ [0..i] ++ [i-1,i-2..0]) | i <- [0..n] ++ [n-1,n-2..0] ]
