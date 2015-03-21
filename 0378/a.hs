main :: IO ()
main = getLine >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Int] -> [Int]
solve [a, b] = [ length [ k | k <- [1..6], abs (k - a) <  abs (k - b) ],
                 length [ k | k <- [1..6], abs (k - a) == abs (k - b) ],
                 length [ k | k <- [1..6], abs (k - a) >  abs (k - b) ] ]
solve _ = undefined
