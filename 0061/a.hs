import Data.Bits (xor)
import Data.Char (digitToInt)

main :: IO ()
main = getContents >>= putStrLn . concatMap show . solve . map (map digitToInt) . lines

solve :: [[Int]] -> [Int]
solve [xs, ys] = zipWith xor xs ys
solve _ = undefined
