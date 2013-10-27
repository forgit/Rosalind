{-
Given: A file containing at most 1000 lines.
Return: A file containing all the even-numbered 
lines from the original file. Assume 1-based numbering of lines.
-}

import System.IO  

onlyEven [] = []
onlyEven [x] = []
onlyEven (x:y:xs) = [y] ++ onlyEven xs

main = do
	input <- readFile "ini5_input.txt"
	writeFile "ini5_output.txt" (unlines $ onlyEven (lines input))
	--print $ unlines $ onlyEven (lines input)