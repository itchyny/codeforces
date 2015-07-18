import Data.Functor ((<$>))
import Data.List (tails)
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = print =<< solve <$> getLine

solve :: String -> Int
solve xs = sum [ fromMaybe 0 $ ((length xs - 3)-) <$> listToMaybe (filter (>=i) bearIndices) | (i, _) <- zip [0..] xs ]
  where bearIndices = [ i | (i, ys) <- zip [0..] (tails xs), take 4 ys == "bear" ]
