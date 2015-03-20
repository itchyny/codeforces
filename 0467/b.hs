import Data.Bits (xor, popCount)

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (_:_:k:xs) = length [ x | x <- init xs, popCount (x `xor` last xs) <= k ]
solve _ = undefined
