import Control.Applicative ((<$>), (<*>))
import Data.Char (intToDigit)

main :: IO ()
main = print =<< solve <$> (read . (!!1) . words <$> getLine) <*> (lines <$> getContents)

solve :: Int -> [String] -> Int
solve k xs = length [ x | x <- xs, all (`elem`x) ['0'..intToDigit k] ]
