{-
Given: A DNA string s of length at most 100 bp and an array A containing at most 20 numbers between 0 and 1.

Return: An array B having the same length as A in which B[k] 
represents the common logarithm of the probability that 
a random string constructed with the GC-content 
found in A[k] will match s exactly.
-}

probability input s = map (logBase 10) zz
    where
        zz      = zipWith (*) aa gg     
        aa      = map (** atpow) at
        gg      = map (** cgpow) gc
        cgpow   = fromIntegral . length $ filter (`elem` ['C','G']) s
        atpow   = fromIntegral . length $ filter (`elem` ['A','T']) s
        gc      = map (* 0.5) input
        at      = [(1 - a) * 0.5 | a <- input]

main = do
    input_raw <- readFile "rosalind_prob.txt"    
    let input_l = lines input_raw
        s = input_l!!0
        a = input_l!!1
    let input   = map (\x -> read x :: Double) (words a)
        res     = probability input s
    putStrLn $ unwords . map (show) $ res