{-
Given: A protein string P of length at most 1000 aa.
Return: The total weight of P. Consult the monoisotopic mass table.
-}

import Fasta (formTuple)


findSum :: [(String, String)] -> String -> Double
findSum [] _ = error "No element in list"
findSum ((t,y):ts) s 
    | s == t    = read y :: Double
    | otherwise = findSum ts s

main = do
    input_raw <- readFile "rosalind_prtm.txt"
    table_raw <- readFile "Monoisotopic-mass-table.txt"
    let table = formTuple $ words table_raw
        input_raw' = (words input_raw)!!0
        input = map (\x -> [x]) input_raw'
    print $ sum $ map (findSum table) input
