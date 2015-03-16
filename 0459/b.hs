import Data.List (genericLength)

main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words . (!!1) . lines

solve :: [Integer] -> [Integer]
solve bs = [ maximum bs - minimum bs, if maxbs == minbs then lenmax * (lenmax - 1) `div` 2 else lenmax * lenmin ]
  where maxbs = maximum bs
        minbs = minimum bs
        lenmax = genericLength (filter (==maxbs) bs)
        lenmin = genericLength (filter (==minbs) bs) 
