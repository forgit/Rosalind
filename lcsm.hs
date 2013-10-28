import Fasta (splitStr, formTuple, removeRosalind)
import Data.List

subStr x y
    | length x < length y   = subStr' y x
    | otherwise             = subStr' x y

subStr' x [] = ""
subStr' x yy@(y:ys)
    | yy `isInfixOf` x      = yy
    | ys `isInfixOf` x      = ys
    | inityy `isInfixOf` x  = inityy
    | otherwise             = subStr' x (init ys)
    where
        inityy = init yy

commonSubStr (x:xs) (y:ys)
    | otherwise = 0

main = do
    input <- readFile "rosalind_lcsm.txt"
    let 
        s   = splitStr input
        tmp = formTuple $ lines s 
        tmp'= removeRosalind tmp
    print tmp'

    print $ subStr "ACGTACGT" "AACCGTATAAAACGT"