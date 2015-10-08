import Control.Applicative ((<$>), (<*>), (<|>))
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = solve <$> (head . map read . words <$> getLine) <*> (map read . words <$> getContents) >>= print

solve :: Int -> [Int] -> Int
solve p = fromMaybe (-1) . foldl (<|>) Nothing . snd . mapAccumL f [] . zip [1..] . map (`mod` p)
  where f xs (i, x) = (x:xs, listToMaybe [ i | x `elem` xs ])
