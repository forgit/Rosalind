module Main where

{-
Given: A DNA string s of length at most 1000 nt.

Return: Four integers (separated by spaces) counting the respective 
number of times that the symbols 'A', 'C', 'G', and 'T' occur in s.
-}    

s = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

countInit s = count s (0,0,0,0)

count [] (cur_numA,cur_numC,cur_numG,cur_numT) = (cur_numA,cur_numC,cur_numG,cur_numT)
count (x:xs) (cur_numA,cur_numC,cur_numG,cur_numT)
    | x == 'A'  = count xs (cur_numA+1,cur_numC,cur_numG,cur_numT)
    | x == 'C'  = count xs (cur_numA,cur_numC+1,cur_numG,cur_numT)
    | x == 'G'  = count xs (cur_numA,cur_numC,cur_numG+1,cur_numT)
    | x == 'T'  = count xs (cur_numA,cur_numC,cur_numG,cur_numT+1)
    | otherwise = count xs (cur_numA,cur_numC,cur_numG,cur_numT)



main = do
    print $ countInit s