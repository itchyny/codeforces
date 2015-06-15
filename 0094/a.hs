import Control.Applicative ((<$>), (<*>))
import Data.List (elemIndex, unfoldr)
import Data.Maybe (catMaybes)

main :: IO ()
main = putStrLn <$> concatMap show =<< solve <$> getLine <*> (lines <$> getContents)

solve :: String -> [String] -> [Int]
solve xs xss = catMaybes $ flip elemIndex xss <$> splitN 10 xs

splitN :: Int -> [a] -> [[a]]
splitN n = takeWhile (not . null) . unfoldr (Just . splitAt n)
