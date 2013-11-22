import Text.Regex.PCRE
import Data.Array
import Data.List
import Fasta


getIdx str regx = 
   nub . concat . map elems $ matchAll (makeRegex regx :: Regex) str

revComp = comp . reverse
comp [] = []
comp (x:xs)
    | x == 'A'  = ['T'] ++ comp xs
    | x == 'T'  = ['A'] ++ comp xs
    | x == 'C'  = ['G'] ++ comp xs
    | x == 'G'  = ['C'] ++ comp xs
    | otherwise = [x] ++ comp xs    

isEqual [] [] = []
isEqual (x:xs) (y:ys)
    | x == y    = "1" ++ isEqual xs ys
    | otherwise = "0" ++ isEqual xs ys

isPalindrom s = getIdx (isEqual (revComp s) s) "1{4,12}"  -- "11111" of length 4 to 12

getpalindrome s a = nub . concat . map (\(x,y) -> (map (\(a,b) -> (a+y+1,b)) x)) $ f
    where 
        f = filter (\(x,y) -> not . null $ x) t
        t = tmp 0 s
        tmp i s
            | n >= 4 = [(concat $ map (isPalindrom) (map (\n -> take n s) [4..12]), i)] ++ (tmp (i+1) $ drop 1 s)
            | otherwise = []
            where n = length s


s = "TCAATGCATGCGGGTCTATATGCAT"

main = do

--    print $ nub $ concat $ map (\(x,y) -> (map (\(a,b) -> (a+y+1,b)) x)) f
    print $ getpalindrome s (revComp s)


