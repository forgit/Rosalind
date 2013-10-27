{-
Given: Two DNA strings s and t (each of length at most 1 kbp).
Return: All locations of t as a substring of s.
-}

findSubStr [] t curpos pos = pos
findSubStr s t curpos pos
    | str == t  = findSubStr (tail s) t (curpos+1) (pos ++ [curpos])
    | otherwise = findSubStr (tail s) t (curpos+1) (pos)
    where
        str = take (length t) s


s = "GATATATGCATATACTT"
t = "ATAT"

printElements [] = putStrLn ""
printElements (x:xs) = do 
    putStr (show x++" ")
    printElements xs

main = do
    file <- readFile "rosalind_subs.txt"
    let
        input   = lines file
        res     = findSubStr (input!!0) (input!!1) 1 []
    printElements res