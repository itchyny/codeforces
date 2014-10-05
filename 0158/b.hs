import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = B.getContents >>= print . solve . map (fst . fromJust . B.readInt) . B.words . (!!1) . B.lines

solve :: [Int] -> Int
solve = go . count (0, 0, 0, 0)
  where go (0, 0, 0, n) = (n + 3) `div` 4
        go (0, 0, n, k) = (n + 1) `div` 2 + go (0, 0, 0, max 0 (k - 2 * (n `mod` 2)))
        go (0, n, k, l) = n + go (0, 0, k, max 0 (l - n))
        go (n, k, l, m) = n + go (0, k, l, m)
        count (a, b, c, d) (4:ss) = count (a + 1, b, c, d) ss
        count (a, b, c, d) (3:ss) = count (a, b + 1, c, d) ss
        count (a, b, c, d) (2:ss) = count (a, b, c + 1, d) ss
        count (a, b, c, d) (1:ss) = count (a, b, c, d + 1) ss
        count x _ = x
