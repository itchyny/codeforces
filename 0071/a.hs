main :: IO ()
main = getContents >>= mapM_ (putStrLn . solve) . tail . lines

solve :: String -> String
solve cs | length cs <= 10 = cs
         | otherwise = [head cs] ++ show (length cs - 2) ++ [last cs]
