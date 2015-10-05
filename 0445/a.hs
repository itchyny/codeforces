import Data.Functor ((<$>))
import Data.List (tails)

main :: IO ()
main = getLine >> solve <$> (lines <$> getContents) >>= mapM_ putStrLn

solve :: [String] -> [String]
solve = zipWith (zipWith $ \x y -> if y == '-' then y else x) $ tails (cycle "BW")
