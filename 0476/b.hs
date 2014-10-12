import qualified Data.Map as M

main :: IO ()
main = getContents >>= print . solve . lines

solve :: [String] -> Double
solve [s1, s2] = M.findWithDefault 0 (fst $ head $ M.toList m1) m2
  where m1 = solve' s1 (M.singleton 0 1)
        m2 = solve' s2 (M.singleton 0 1)
solve _ = undefined

solve' :: String -> M.Map Integer Double -> M.Map Integer Double
solve' ('+':cs) m = solve' cs $ M.mapKeys succ m
solve' ('-':cs) m = solve' cs $ M.mapKeys pred m
solve' ('?':cs) m = solve' cs $ M.unionWith (+) (M.mapKeys pred m') (M.mapKeys succ m')
  where m' = M.map (/2) m
solve' _ m = m
