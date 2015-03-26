import Data.Char (toLower)
import qualified Data.Map as M

main :: IO ()
main = getLine >> getLine >>= print . solve M.empty . map toLower

solve :: M.Map Char Int -> String -> Int
solve m (y:z:ys) | y == z = solve m ys
                 | M.findWithDefault 0 z m > 0 = solve (M.insertWith (+) y 1 (M.adjust pred z m)) ys
                 | otherwise = 1 + solve (M.insertWith (+) y 1 m) ys
solve _ _ = 0
