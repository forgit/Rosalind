binomial :: Double -> Int -> Int -> Double
binomial p n k = p^k * (1-p)^(n-k) * fromIntegral(choose n k)

factorial :: Int -> Integer
factorial n = product [1..fromIntegral n]

choose :: Int -> Int -> Integer
choose n k = factorial n `div` (factorial k * factorial (n-k))

lia :: Int -> Int -> Double
lia k n = 1 - (sum $ map (binomial 0.25 (2^k)) [0..n-1])

main = do
    readFile "rosalind_lia.txt" >>= 
        return . map (\x -> read x :: Int) . words >>= \[n,k] ->
            print $ lia n k 