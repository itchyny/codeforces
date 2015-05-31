import Control.Applicative ((<$>), (<*>))
import qualified Data.Array as A

main :: IO ()
main = print =<< solve <$> readLn <*> (map read <$> lines <$> getContents)

solve :: Integer -> [Integer] -> Integer
solve n ps = maximum $ A.elems dp
  where ps' = A.listArray (1, n) ps
        dp = A.array (1, n) [ (i, if ps' A.! i == -1 then 1 else 1 + dp A.! (ps' A.! i)) | i <- [1..n] ]
