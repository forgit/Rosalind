import Fasta (splitStr, formTuple, removeRosalind)
import Data.List
import Data.Function
 
lcstr xs ys = maximumBy (compare `on` length) . concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]
  where f xs ys = scanl g [] $ zip xs ys
        g z (x, y) = if x == y then z ++ [x] else []

commonSubStr = nub . words . unwords . noFalseLst

zzz xs = zip (bbb xs (aaa xs xs)) (aaa xs xs) 

aaa [] _ = []
aaa (x:xs) xx = map (lcstr x) (filter (/= x) xx) ++ aaa xs xx

bbb _ [] = []
bbb xx (y:ys) = [map (y `isInfixOf`) xx]++ bbb xx ys

noFalse (xx@(x:xs),y)
    | not $ False `elem` xx = y
    | otherwise             = ""

noFalseLst [] = []
noFalseLst (x:xs) = [noFalse x] ++ noFalseLst xs

main = do
    input <- readFile "rosalind_lcsm.txt"
    let 
        s   = splitStr input
        tmp = formTuple $ lines s 
        tmp'= nub $ removeRosalind tmp
        res = commonSubStr $ zzz tmp'
    putStrLn $ res!!0



