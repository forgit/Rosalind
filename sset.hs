{-
Given: A positive integer n (n≤1000).

Return: The total number of subsets of {1,2,…,n} modulo 1,000,000.
-}

main = readFile "rosalind_sset.txt" >>=
    print . (\n -> (2 ^ (read n)) `mod` 1000000)