import Control.Applicative ((<$>))
import Data.List (group)

main :: IO ()
main = print =<< solve <$> getLine

solve :: String -> Int
solve = sum . map ((`div`5) . (+4) . length) . group
