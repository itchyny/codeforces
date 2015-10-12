import Control.Applicative ((<$>), (<*>))
import Data.List (sort)

main :: IO ()
main = putStrLn =<< solve <$> getLine <*> getLine

solve :: Ord a => [a] -> [a] -> String
solve s t | t `isContainedIn` s = "automaton"
          | sort s == sort t = "array"
          | sort t `isContainedIn` sort s = "both"
          | otherwise = "need tree"

isContainedIn :: Eq a => [a] -> [a] -> Bool
isContainedIn xxs@(x:xs) (y:ys) | x == y = isContainedIn xs ys
                                | otherwise = isContainedIn xxs ys
isContainedIn [] _ = True
isContainedIn _ _ = False
