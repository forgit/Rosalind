{-
Given: Three positive integers k, m, and n, 
representing a population containing k+m+n organisms: 
k individuals are homozygous dominant for a factor, 
m are heterozygous, and n are homozygous recessive.

Return: The probability that two randomly selected 
mating organisms will produce an individual possessing 
a dominant allele (and thus displaying the dominant phenotype). 
Assume that any two organisms can mate.
-}

{-
 example: 2 2 2
 2 homozygous dominant (AA), 2 heterozygous (Aa), and 2 homozygous recessive (aa)
 P(dominant) = (P(AA + AA) + P(AA + Aa) + P(Aa + Aa) + P(Aa + aa) + P(AA + aa)) / C(6,2)
 = (1 + 4 + 1 * 0.75 + 4 * 0.5 + 4) / (6! / 2! * 4!)
 = 0.78333
-}

-- fast check if length of list <=n
fastCheck n = null . drop n

factorial n = product [1..n]

c n k = factorial n / ((factorial k) * (factorial (n - k)))

prob k m n = p
    where 
        s = [
            k * (k - 1),        --AA, AA
            k * m,              --AA, Aa
            k * n,              --AA, aa
            m * k,              --Aa, AA
            m * (m - 1) * 0.75, --Aa, Aa
            m * n * 0.5,        --Aa, aa
            n * k,              --aa, AA
            n * m * 0.5        --aa, Aa
            ]
        f = k + m + n
        p = sum s / (f * (f - 1))

main = do
    input <- readFile "rosalind_iprb.txt"
    let 
        (k:m:n:xs) = words input
    print $ prob (read k) (read m) (read n)