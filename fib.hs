{-
Given: Positive integers n≤40 and k≤5.
Return: The total number of rabbit pairs that will be present 
after n months if we begin with 1 pair and in each generation, 
every pair of reproduction-age rabbits produces 
a litter of k rabbit pairs (instead of only 1 pair).
-}


fib 0 _= 0
fib 1 _ = 1
fib n k = fib (n-1) k + k*fib (n-2) k

main = do

    print $ fib 28 4