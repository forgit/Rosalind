{-
Given: A collection of n (n≤10) DNA strings s1,…,sn of equal length (at most 1 kbp). Strings are given in FASTA format.
Return: The matrix D corresponding to the p-distance dp on the given strings. As always, note that your answer is allowed an absolute error of 0.001.
-}

import Fasta (parse)
import Data.List

d s1 s2 = (sum $ zipWith (comp) s1 s2) / (fromIntegral $ length s1)
    where 
        comp a b
            | a == b = 0
            | otherwise = 1

create l = [ [d x y | x <- l] | y <- l]

printM = mapM_ (putStrLn . unwords . map show)

main = do
    printM . create . map (\(x, y)-> y) . parse . lines =<< readFile "rosalind_pdst.txt"