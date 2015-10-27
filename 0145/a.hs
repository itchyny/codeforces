import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))

main :: IO ()
main = print =<< solve <$> (zip <$> getLine <*> getLine)

solve :: [(Char, Char)] -> Int
solve = uncurry max <$> (count ('7', '4') &&& count ('4', '7'))

count :: Eq a => a -> [a] -> Int
count a = length . filter (==a)
