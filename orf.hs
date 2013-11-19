import Data.List.Utils (replace)
import Fasta (formTuple,removeRosalind,parse)
import Data.Maybe (fromJust)
import Data.List (nub)
import Text.Regex.PCRE (matchAll, makeRegex, Regex)
import Data.Array (elems)

dna2Rna = replace "T" "U"

rna2Prot [] table = []
rna2Prot s table = (fromJust $ lookup (take 3 s) table) ++ rna2Prot (drop 3 s) table

revComp = comp . reverse
comp [] = []
comp (x:xs)
    | x == 'A'  = ['T'] ++ comp xs
    | x == 'T'  = ['A'] ++ comp xs
    | x == 'C'  = ['G'] ++ comp xs
    | x == 'G'  = ['C'] ++ comp xs
    | otherwise = [x] ++ comp xs 

getIdx str regex = 
   nub . map fst . concat . map elems $ matchAll (makeRegex regex :: Regex) str

orf dna table = map (\(x,y) -> rna2Prot (take (y-x) $ drop x rna) table) $ map (\(x,y)-> (x,head y)) $ filter (\(x,y)->not $ null y) startStops
    where
        rna = dna2Rna dna
        starts = getIdx rna "AUG"
        stops = getIdx rna "(UAG|UGA|UAA)"
        startStops = map (\x -> 
            (x, filter (\y -> (y>x) && (y-x) `mod` 3 == 0) stops)
            ) starts

main = do
    readFile "rosalind_orf.txt" >>= return . head . removeRosalind . parse . lines >>= \dna ->    
        readFile "RNA_table.txt" >>= return . formTuple . words >>= \table ->
            putStrLn . unlines . nub $ orf dna table ++ orf (revComp dna) table