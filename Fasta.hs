module Fasta (splitStr, formTuple, removeRosalind) where

splitStr []     = ""
splitStr [x]    = [x]
splitStr [x,y]  = [x,y]
splitStr (x:y:z:xs)
    | x `elem` ['C','G','T','A'] && y == '\n' && z `elem` ['C','G','T','A'] = splitStr (x:z:xs)
    | otherwise     = [x] ++ splitStr (y:z:xs)

formTuple [] = []
formTuple (x:y:xs) = [(x,y)] ++ formTuple (xs)

removeRosalind [] = []
removeRosalind ((x,y):xs) = [y] ++ removeRosalind xs