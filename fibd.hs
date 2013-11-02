{-
Given: Positive integers n≤100 and m≤20.

Return: The total number of pairs of rabbits that 
will remain after the n-th month if all rabbits live for m months.
-}

rabbitsRemain n m = last t 
    where t = 1:1:1:[t!!(k-1)+t!!(k-2) | k<-[3..m]]
            ++ [t!!(k-1)+t!!(k-2)-t!!(k-m-1) | k<-[m+1..n]]

main = do
    input <- readFile "rosalind_fibd.txt"
    let 
        (n:m:xs) = words input
    print $ rabbitsRemain (read n) (read m)

