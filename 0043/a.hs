import Data.List (group, maximumBy, sort)
import Data.Ord (comparing)

main :: IO ()
main = getContents >>= putStrLn . solve . tail . lines

solve :: [String] -> String
solve = head . maximumBy (comparing length) . group . sort
