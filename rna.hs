{-
Given: A DNA string t having length at most 1000 nt.
Return: The transcribed RNA string of t.
-}

t = "GATGGAACTTGACTACGTAAATT"

replaceTU [] = []
replaceTU (x:xs)
    | x == 'T'  = ['U'] ++ replaceTU xs
    | otherwise = [x] ++ replaceTU xs

main = do
    print $ replaceTU t
