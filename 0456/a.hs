import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import Data.Maybe (fromJust)

main :: IO ()
main = B.getContents >>= putStrLn . output . solve . map (map (fst . fromJust . B.readInt) . B.words) . tail . B.lines

solve :: [[Int]] -> Bool
solve = or . (\xs -> zipWith (>) xs (tail xs)) . map (!!1) . sort

output :: Bool -> String
output True = "Happy Alex"
output _ = "Poor Alex"
