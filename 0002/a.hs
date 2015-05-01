import Control.Arrow (second)
import Data.List (mapAccumL)
import qualified Data.Map as M

main :: IO ()
main = getContents >>= putStrLn . solve . map (second read . toTuple . words) . tail . lines

solve :: [(String, Integer)] -> String
solve xs = head [ name | (name, score) <- scores, M.member name winners, score >= maxScore ]
  where (lastScores, scores) = mapAccumL f M.empty xs
        f s (x, i) = (M.insertWith (+) x i s, (x, M.findWithDefault 0 x s + i))
        maxScore = maximum (M.elems lastScores)
        winners = M.filter (==maxScore) lastScores

toTuple :: [a] -> (a, a)
toTuple xs = (head xs, xs !! 1)
