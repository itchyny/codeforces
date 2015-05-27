import Control.Arrow ((&&&))
import Data.List (group, sort)

main :: IO ()
main = getContents >>= printSolution . solve . map read . tail . words

solve :: [Int] -> [Int]
solve = uncurry (++) . (map head &&& concatMap (tail . take 2) . tail . reverse) . group . sort

printSolution :: Show a => [a] -> IO ()
printSolution xs = print (length xs) >> putStrLn (unwords (map show xs))
