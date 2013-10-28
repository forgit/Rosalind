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


commonSubs' a b
    | length a < length b   = b `csubs` a
    | otherwise       = a `csubs` b


-- s - shorterst string
test [] s =[]
test (x:y:ys) s
    | x=="1" && y=="1" = "" ++ test (ys)
    | otherwise        = test ys

{-
    THE LARGEST SET OF ZEROS (IN THE SORTEST STRING) 
    CORRESPONDS TO COMMON SUBSTRING
-}
-- bb is shorter
aa `csubs` bb 
    | na >= nb  = [bb `subs` (take nb aa)] ++ (tail aa `csubs` bb)
    | otherwise = []
    where 
        nb = length bb
        na = length aa

-- if equal length
[] `subs` [] = ""
(a:as) `subs` (b:bs) = (a `sub` b) ++ as `subs` bs

a `sub` b
    | a==b = "0"
    | otherwise = "1"



--ss xx@(x:xs) yy@(y:ys)
--    | y `isInfixOf` xx = [y] ++ ys

main = do
    input <- readFile "rosalind_lcsm.txt"
    let 
        s   = splitStr input
        tmp = formTuple $ lines s 
        tmp'= removeRosalind tmp
    print tmp'

    --print $ subStr "ACGTACGT" "AACCGTATAAAACGT"
    print $ "AACCGTATAAAACGT" `csubs` "GTATACAACG"
    print $ commonSubs' "GTATACAACG" "AACCGTATAAAACGT"

