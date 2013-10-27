{-
Given: A DNA string s of length at most 1000 bp.
Return: The reverse complement sc of s.
-}

s = "AAAACCCGGT"

revComp = comp . reverse

comp [] = []
comp (x:xs)
    | x == 'A'  = ['T'] ++ comp xs
    | x == 'T'  = ['A'] ++ comp xs
    | x == 'C'  = ['G'] ++ comp xs
    | x == 'G'  = ['C'] ++ comp xs
    | otherwise = [x] ++ comp xs    

main = do
    print $ revComp s
