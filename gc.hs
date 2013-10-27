{-
Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).

Return: The ID of the string having the highest GC-content, 
followed by the GC-content of that string. Rosalind allows 
for a default error of 0.001 in all decimal answers 
unless otherwise stated; 
please see the note on absolute error below.
-}

t = "AGC\nTATAG"

removeGC [] = []
removeGC (x:xs)
    | x == 'G' || x == 'C'  = [x] ++ removeGC xs
    | otherwise             = removeGC xs


contentGC s = (fromIntegral $ length $ removeGC s) / (fromIntegral $ length s)

formTuple [] = []
formTuple (x:y:xs) = [(x,contentGC y)] ++ formTuple (xs)


maxInTuple [] (curMaxStr, curMax) = (curMaxStr, curMax)
maxInTuple ((x,y):xs) (curMaxStr, curMax)
    | y > curMax    = maxInTuple xs (x,y)
    | otherwise     = maxInTuple xs (curMaxStr, curMax)


concGC [] = []
concGC (x:xs) = concTwo x $ head xs ++ concGC xs


concTwo s1 s2
    | s1!!0 /= '>' && s2!!0 /= '>'  = s1 ++ s2
    | otherwise                     = []

splitStr []     = ""
splitStr [x]    = [x]
splitStr [x,y]  = [x,y]
splitStr (x:y:z:xs)
    | x `elem` ['C','G','T','A'] && y == '\n' && z `elem` ['C','G','T','A'] = splitStr (x:z:xs)
    | otherwise     = [x] ++ splitStr (y:z:xs)

main = do
    input <- readFile "rosalind_gc.txt"
    let 
        s   = splitStr input
        tmp = formTuple $ lines s
        (tmp', max) = maxInTuple tmp ("",0)
    putStrLn $ [x | x<-tmp', x /= '>']
    print $ 100 * max

