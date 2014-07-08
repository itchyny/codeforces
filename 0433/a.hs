import Control.Arrow ((&&&))
import Data.Functor ((<$>))

main :: IO ()
main = putStrLn =<< yesno . solve . tail . map read . words <$> getContents

solve :: [Integer] -> Bool
solve = (\(a, b) -> even a && (a /= 0 || even b)) . (length . filter (==100) &&& length . filter (==200))

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
