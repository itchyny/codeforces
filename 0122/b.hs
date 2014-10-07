import Data.List (group, inits, minimumBy, sort, tails)
import Data.Function (on)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve cs = solve' [ c | c <- succsequences cs, head c /= '0', isLucky c ]
   where solve' [] = "-1"
         solve' xs = head $ minimumBy (compare `on` (negate . length)) $ group $ sort xs

isLucky :: String -> Bool
isLucky = all (`elem`"47")

succsequences :: [a] -> [[a]]
succsequences = concatMap (tail . inits) . tails
