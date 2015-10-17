import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = print =<< solve <$> readLn <*> (map read <$> words <$> getLine)

solve :: Int -> [Int] -> Int
solve _ as | null r = 0
           | all (uncurry (<=)) (tail r) && snd (last r) <= head as = length r
           | otherwise = -1
  where r = dropWhile (uncurry (<=)) (zip as (tail as))
