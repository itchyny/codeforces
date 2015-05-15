import Control.Applicative ((<$>), (<*>))
import Data.Char (isDigit)
import Data.List (group, sort)

main :: IO ()
main = putStrLn . yesno =<< solve <$> readLn <*> getContents

solve :: Int -> String -> Bool
solve k = all (<=2*k) . map length . group . filter isDigit . sort

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
