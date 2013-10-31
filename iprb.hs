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


-- fast check if length of list <=n
fastCheck n = null . drop n

k=2
m=2
n=2

factorial n = product [1..n]



main = do
    print $ fastCheck 10 [1..100]