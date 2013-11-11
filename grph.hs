{-
Given: A collection of DNA strings 
in FASTA format having total length at most 10 kbp.

Return: The adjacency list corresponding to O3. 
You may return edges in any order.
-}

import Fasta(splitStr, formTuple)

overlap k (a,b) (x,y)
    | t == h && a /= x = (a,x)
    | otherwise = ("","")
    where   h = take k y
            t = drop (n-k) b
            n = length b

printList [] = putStr ""
printList ((x,y):xs)
    | x/="" = do 
        putStrLn $ (drop 1 x) ++ " " ++ (drop 1 y)
        printList xs
    | otherwise = printList xs

grph k s = concat $ map (\(x,y) -> map (overlap k (x,y)) s) s

main = do
    let k = 3
    readFile "rosalind_grph.txt" >>= 
        printList . (grph k) . formTuple . lines . splitStr