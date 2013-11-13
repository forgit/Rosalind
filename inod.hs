{-
Given: A positive integer n (3≤n≤10000).

Return: The number of internal nodes of 
any unrooted binary tree having n leaves.
-}

main = do
    readFile "rosalind_inod.txt" >>=
        print . (\x -> x - 2) . (\x -> read x :: Int)
