import Data.Ratio

main :: IO ()
main = getContents >>= putStrLn . format . solve . map read . words

solve :: [Integer] -> Ratio Integer
solve xs = (7 - maximum xs) % 6

format :: Ratio Integer -> String
format r = concat [ show $ numerator r, "/", show $ denominator r ]
