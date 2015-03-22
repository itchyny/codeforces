import Data.List (intercalate, nub, sortBy, unfoldr)
import Data.Char (isDigit)

main :: IO ()
main = getContents >>= mapM_ putStrLn . solve . tail . lines

data Phone = Phone { name :: String, taxi :: Int, pizza :: Int, girl :: Int } deriving Show

solve :: [String] -> [String]
solve = solve' . unfoldr f
  where f [] = Nothing
        f (x:xs) = Just (classify n numbers, rest)
          where [ si, n ] = words x
                (numbers, rest) = splitAt (read si) xs

solve' :: [Phone] -> [String]
solve' phones = zipWith (++) prefixes [ (++".") $ intercalate ", " $ map name
              $ filter ((==maximum (map f phones)) . f) phones | f <- [ taxi, pizza, girl ] ]
  where prefixes = [ "If you want to call a taxi, you should call: ",
                     "If you want to order a pizza, you should call: ",
                     "If you want to go to a cafe with a wonderful girl, you should call: " ]

isTaxi :: String -> Bool
isTaxi xs = length (nub (filter isDigit xs)) == 1

isPizza :: String -> Bool
isPizza xs = sortBy (flip compare) (nub xs') == xs'
  where xs' = filter isDigit xs

classify :: String -> [String] -> Phone
classify n xs = Phone n t p (length xs - t - p)
  where t = length (filter isTaxi xs)
        p = length (filter isPizza xs)
