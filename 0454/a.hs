main :: IO ()
main = readLn >>= mapM_ putStrLn . solve

solve :: Int -> [String]
solve n = [ replicate (m - i) '*' ++ replicate (2 * i + 1) 'D'
         ++ replicate (m - i) '*' | i <- [0..m] ++ [m-1, m-2 .. 0] ]
  where m = n `div` 2
