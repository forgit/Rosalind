import Data.List (nub,tails)
import Fasta (parse, removeRosalind)

revComp = comp . reverse
comp [] = []
comp (x:xs)
    | x == 'A'  = ['T'] ++ comp xs
    | x == 'T'  = ['A'] ++ comp xs
    | x == 'C'  = ['G'] ++ comp xs
    | x == 'G'  = ['C'] ++ comp xs
    | otherwise = [x] ++ comp xs    

printM = mapM_ (putStrLn . (\(x,y) -> show x ++ " " ++ show y))

isPalindrom' s = s == revComp s

getPalindromes s = 
    concat $ 
        map (\(x,y) -> map (\(a,b) -> (x,a)) y) $ 
            filter (\(x,y) -> not . null $ y) $ 
                map (\(x,y) -> (x, filter (\(x,y) -> y == True ) y)) $ 
                    zip [1..] $ map (zip [4..12]) $ 
                        map (\s -> map (isPalindrom') $ nub (map (\n ->  take n s) [4..12])) cutt

        where
            t = tails s
            cutt = take (length t - 4) t



main = readFile "rosalind_revp.txt" >>= 
    printM . getPalindromes . head . removeRosalind . parse . lines


