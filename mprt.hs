import Network.HTTP.Wget
import Data.List

parse :: [String] -> [(String, String)]
parse [] = []
parse xx = [(head xx, concat x)] ++ parse y
    where   
        (x, y) = break ('>' `elem`) $ tail xx

glyco [n,p,st,_p]
    | n == 'N' && p /= 'P' && 
        (st == 'S' || st == 'T') && _p /= 'P' = True
    | otherwise = False

match str = findmatch 0 (length str) [] str
    where   
        findmatch i 3 acc _ = reverse acc
        findmatch i n acc xs
            | glyco t = findmatch (i+1) (n-1) ([i+1] ++ acc) $ tail xs 
            | otherwise = findmatch (i+1) (n-1) acc $ tail xs
                where   t = take 4 xs

comp = concat . map (match . snd) . parse . lines

formURL [] = []
formURL (x:xs) = [(x, url ++ x ++ ".fasta")] ++ formURL xs

wrap [] = return ()
wrap ((id, url):ss) = do
    a <- wget url [] []
    let t = comp a
    if null t 
        then 
            wrap ss
        else do
            putStrLn id
            putStrLn . unwords . map show $ t
            wrap ss

url = "http://www.uniprot.org/uniprot/"            

main = do
    wrap . formURL . lines =<< readFile "rosalind_mprt.txt"

