import Control.Applicative ((<$>), (<|>))
import Data.Array ((//), elems, listArray)
import Data.Char (digitToInt)
import Data.List (findIndex, findIndices)
import Data.Maybe (listToMaybe)

main :: IO ()
main = solve <$> (map digitToInt <$> getLine) >>= putStrLn

solve :: [Int] -> String
solve xs = maybe "-1" f $ findIndex (\c -> even c && c < last xs) xs <|> listToMaybe (reverse $ findIndices even xs)
  where f i = concatMap show $ elems $ listArray (1, length xs) xs // [ (i + 1, last xs), (length xs, xs !! i) ]
