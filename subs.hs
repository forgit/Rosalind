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

main = do
    print $ findSubStr s t 1 []
