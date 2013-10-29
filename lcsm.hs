import Fasta (splitStr, formTuple, removeRosalind)
import Data.List
import Data.Function
 
lcstr xs ys = maximumBy (compare `on` length) . concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]
  where f xs ys = scanl g [] $ zip xs ys
        g z (x, y) = if x == y then z ++ [x] else []

commonSubStr xx@(x:xs) = scanl (lcstr) (head xx) (tail xx)

applyFst x xs = map (lcstr x) xs

apply [] = []
--apply x = (map (lcstr x) xs) ++ apply xs
--apply (x:xs) = (map (lcstr x) xs) ++ apply xs

aaa [] _ = []
aaa (x:xs) xx = map (lcstr x) (filter (/= x) xx) ++ aaa xs xx

main = do
    input <- readFile "rosalind_lcsm.txt"
    let 
        s   = splitStr input
        tmp = formTuple $ lines s 
        tmp'= nub $ removeRosalind tmp

    let 
        cs = lcstr "GATTACA" "TAGACCA"

    print $ tmp'
    print $ cs
    print $ commonSubStr tmp'
    print $ applyFst (head tmp') (tail tmp')
    --print $ filter (/= head tmp') tmp'
    print $ aaa tmp' tmp'
--    print $ scanl (lcstr) (head tmp') (tail tmp')


