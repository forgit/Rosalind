{-
Given: A string s of length at most 10000 letters.
Return: How many times any word occurred in string. 
Each letter case (upper or lower) in word matters. 
Lines in output can be in any order.
-}

import Data.List  
import Control.Arrow ((&&&))

s = "We tried list and we tried dicts also we tried Zen"

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length) . group . sort . words    

main = do
    print $ wordCount s

    

