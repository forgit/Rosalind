{-
Given: Positive integers n≤100 and m≤20.

Return: The total number of pairs of rabbits that 
will remain after the n-th month if all rabbits live for m months.
-}

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

rabbitsRemain n m
    | n == 1 || n == 2 = 1
    | n <= m = rabbitsRemain (n-1) m + rabbitsRemain (n-2) m
    | otherwise = rabbitsRemain (n-1) m + rabbitsRemain (n-2) m - 1

rabbitsRemainList n m l
    | n == 1 = [1]
    | n == 2 = 1:l
    | n == 3 = rabbitsRemainList 2 m l + rabbitsRemainList 1 m l
    | n <= m = rabbitsRemain (n-1) m + rabbitsRemain (n-2) m

fibList 0 = [0]
fibList 1 = 1: fibList 0
--fibList 2 = fibList 1 + fibList 0
--fibList n = fibList (n-1) + fibList (n-2)

takeFibs n = take n fibs
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

rabbitsRL n = last $ take n $ iterate (\(a,b) -> (b,a+b)) (0,1)

rabbitsR n m = last $ map fst $ take (n-1) (iterate (\(a,b) -> (b,a+b-1)) (p,q))
    where
        (p,q) = rabbitsRL m

main = do
    input <- readFile "rosalind_fibd.txt"
    let 
        (n:m:xs) = words input
--    print $ rabbitsRemain (read n) (read m)
--    print $ rabbitsRemain 81 19
--    print $ rabbitsRemainList 30 5 []
--    print $ takeFibs 10
--    print $ take 10 (iterate (\(a,b) -> (b,a+b-1)) (2,2))
    print $ rabbitsR 81 19
    print 37773534761266700
