import Control.Applicative ((<$>), (<*>))
import Data.List (permutations)
import Data.Maybe (isJust)
import Data.Time.Calendar (addGregorianYearsClip, fromGregorianValid, Day)

main :: IO ()
main = putStrLn . yesno =<< solve <$> getLine <*> getLine

solve :: String -> String -> Bool
solve xs ys = or [ check (toDay $ split '.' xs) zs | zs <- map toDay $ permutations $ split '.' ys, isJust zs ]
  where split x = words . map (\c -> if c == x then ' ' else c)

check :: Maybe Day -> Maybe Day -> Bool
check d b = d >= (addGregorianYearsClip 18 <$> b)

toDay :: [String] -> Maybe Day
toDay dmy = fromGregorianValid (read $ dmy !! 2) (read $ dmy !! 1) (read $ head dmy)

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
