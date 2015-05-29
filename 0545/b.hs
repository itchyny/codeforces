import Control.Applicative ((<$>), (<*>))
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = putStrLn <$> fromMaybe "impossible" =<< solve <$> getLine <*> getLine

solve :: String -> String -> Maybe String
solve xs ys = listToMaybe [ snd $ mapAccumL (\z (x, y) -> (z + fromEnum (x /= y), [ x, y ] !! fromEnum (odd z))) 0 $ zip xs ys | even $ length $ filter id $ zipWith (/=) xs ys ]
