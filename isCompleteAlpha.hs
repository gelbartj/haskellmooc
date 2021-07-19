import Data.List
import Data.Char

isCompleteAlpha :: String -> Bool
isCompleteAlpha input = (sort . nub $ map toLower $ filter isAlpha input) == 
    ['a'..'z']