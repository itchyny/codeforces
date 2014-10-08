main :: IO ()
main = readLn >>= putStrLn . solve

solve :: Int -> String
solve = yesno . isMagic . show

isMagic :: String -> Bool
isMagic ('1':'4':'4':xs) = isMagic xs
isMagic ('1':'4':xs) = isMagic xs
isMagic ('1':xs) = isMagic xs
isMagic "" = True
isMagic _ = False

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
