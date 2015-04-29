import Data.List (sort)

main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve [ x, y ] = concat $ sort [ [ 0, signum y * z ], [ signum x * z, 0 ] ]
  where z = abs x + abs y
solve _ = undefined
