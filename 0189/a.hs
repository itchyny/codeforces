import Data.Array.Unboxed ((!), Array, array)
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [ n, a, b, c ] = fromMaybe 0 $ dp ! n
  where dp :: Array Integer (Maybe Integer)
        dp = array (0, n) $ (0, Just 0) : [ (k, succ <$> maximum (Nothing : [ dp ! m | m <- [ k - a, k - b, k - c ], m >= 0])) | k <- [1..n] ]
solve _ = undefined
