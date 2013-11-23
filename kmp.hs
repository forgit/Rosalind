sub j k s = take (k-j+1) $ drop (j-1) s

isEqual j k s = if sub j k s == sub 1 (k-j+1) s then k-j+1 else 0

kmp s = 0: map (maximum . map (\(j,k) -> isEqual j k s)) idx
    where idx = map (\k -> [(j,k) | j <- [2..k]]) [2..length s]


s="CAGCATGGTATCACAGCAGAG"

ans = "0 0 0 1 2 0 0 0 0 0 0 1 2 1 2 3 4 5 3 0 0"

main = do
    putStrLn . unwords . map (show) $ kmp s
    