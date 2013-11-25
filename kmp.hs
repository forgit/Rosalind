import Fasta (removeRosalind, parse)

sub j k s = take (k-j+1) $ drop (j-1) s

isEqual j k s = if sub j k s == sub 1 (k-j+1) s then k-j+1 else 0

kmp s = 0: map (maximum . map (\(j,k) -> isEqual j k s)) idx
    where idx = map (\k -> [(j,k) | j <- [2..k]]) [2..length s]


main = readFile "rosalind_kmp.txt" >>=
    putStrLn . unwords . map (show) . kmp . 
        head . removeRosalind . parse . lines
    