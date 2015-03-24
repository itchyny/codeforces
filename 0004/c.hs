import qualified Data.Map as M
import Data.List (mapAccumL)

main :: IO ()
main = getContents >>= mapM_ putStrLn . solve . tail . lines

solve :: [String] -> [String]
solve = snd . mapAccumL f M.empty
  where f m x = (M.insertWith (+) x (1 :: Integer) m, maybe "OK" ((x++) . show) (M.lookup x m))
