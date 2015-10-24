import Data.Char (digitToInt)

main :: IO ()
main = getContents >>= printSolution . (\[ a, b, c ] -> solve a b c) . map read . words

solve :: Integer -> Integer -> Integer -> [Integer]
solve a b c = [ x | sx <- [1..81], let x = b * sx ^ a + c, 0 < x, x < 1000000000, s x == sx ]

s :: Integer -> Integer
s = sum . map (fromIntegral . digitToInt) . show

printSolution :: Show a => [a] -> IO ()
printSolution xs = print (length xs) >> putStrLn (unwords (map show xs))
