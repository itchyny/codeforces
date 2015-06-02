import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = putStrLn <$> unwords <$> map show =<< solve <$> readLn <*> (map read <$> words <$> getLine)

solve :: Integer -> [Integer] -> [Integer]
solve n as = snd $ minimum [ (abs (a - b), [ i, i `mod` n + 1 ]) | (i, a, b) <- zip3 [1..] as (tail $ cycle as) ]
