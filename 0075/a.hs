import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = putStrLn <$> yesno =<< solve <$> readLn <*> readLn

solve :: Integer -> Integer -> Bool
solve a b = rmZero (a + b) == rmZero a + rmZero b

rmZero :: Integer -> Integer
rmZero = read . filter (/='0') . show

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
