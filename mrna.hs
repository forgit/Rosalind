import Fasta (formTuple)
import Data.List (sort, group)
import Data.Maybe (fromJust)

mrna :: String -> [(String, Integer)] -> Integer
mrna xs table =
    (product $ map (\x -> fromJust $ lookup [x] table) xs) *
        (fromJust $ lookup "Stop" table) `mod` 1000000

main = readFile "rosalind_mrna.txt" >>= \s ->
    print . (mrna . head . lines $ s) . map (\x -> (head x, toInteger $ length x)) . group . sort . 
        map (\(x, y) -> y) . formTuple . words =<< readFile "RNA_table.txt"