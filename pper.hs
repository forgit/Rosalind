{-
Given: Positive integers n and k such that 100≥n>0 and 10≥k>0.

Return: The total number of partial permutations P(n,k), modulo 1,000,000.
-}

p :: Integer -> Integer -> Integer
p n k = product [(n-k+1)..n] `mod` 1000000

main = readFile "rosalind_pper.txt" >>=
    print . foldr1 (p) . map (\x -> read x :: Integer) . words
