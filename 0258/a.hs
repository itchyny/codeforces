main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve xs | all (=='1') xs = tail xs
         | otherwise = takeWhile (=='1') xs ++ tail (dropWhile (=='1') xs)
