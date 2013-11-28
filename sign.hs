import Data.List

permute [] = [[]]
permute xs = concat $ map (\x-> map (x:) (permute (delete x xs))) xs

groupN [] _ = []
groupN xs n = a : groupN b n 
    where (a,b) = splitAt n xs

sign n = nub $ map (\(x,y) -> y) $ filter (\(x,y) -> not x) $ map (\xs -> (False `elem` (map (\ss -> length ss == 1) $ group . sort$ map abs xs),xs)) $ aa 
    where 
        aa = (\xs -> groupN xs n) $ concat $ permutations $ [-n..n]\\[0]

printA xs = mapM_ (putStrLn . unwords . map show) xs


main = do
    let n = 3
        r = sign n
    print $ length r
    printA r
