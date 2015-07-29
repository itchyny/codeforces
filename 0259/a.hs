main :: IO ()
main = putStrLn . yesno . all (`elem`["WBWBWBWB", "BWBWBWBW"]) . lines =<< getContents

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
