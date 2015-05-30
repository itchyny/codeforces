import Data.Function (on)
import Data.Functor ((<$>))
import Data.List (groupBy, inits, permutations, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = printSolution =<< solve <$> tail <$> map read <$> words <$> getContents

solve :: [Integer] -> Maybe [[Integer]]
solve = listToMaybe . drop 3 . inits . map concat . mapM (permutations . map fst)
      . groupBy ((==) `on` snd) . sortBy (compare `on` snd) . zip [1..]

printSolution :: Show a => Maybe [[a]] -> IO ()
printSolution xs = putStrLn (maybe "NO" (const "YES") xs)
                 >> mapM_ (putStrLn . unwords . map show) (fromMaybe [] xs)
