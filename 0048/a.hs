import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = solve <$> getLine <*> getLine <*> getLine >>= putStrLn

solve :: String -> String -> String -> String
solve f m s | f `win` m && f `win` s = "F"
            | m `win` s && m `win` f = "M"
            | s `win` f && s `win` m = "S"
            | otherwise = "?"

win :: String -> String -> Bool
win "rock" "scissors" = True
win "paper" "rock" = True
win "scissors" "paper" = True
win _ _ = False
