main :: IO ()
main = getContents >>= print . maximum . map (read :: String -> Integer) . tail . words
