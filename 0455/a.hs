import Data.List (foldl', genericLength, group, sort)
import Control.Arrow ((&&&))

main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Integer] -> Integer
solve = uncurry max . snd . foldl' f (0, (0, 0)) . map (head &&& genericLength) . group . sort
  where f (a, (b, c)) (d, e) | a + 1 < d = (d, (max (d * e + b) (max b c), max b c))
                             | otherwise = (d, (max (d * e + c) b, b))
