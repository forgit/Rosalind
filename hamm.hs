{-
Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
Return: The Hamming distance dH(s,t).
-}


s="CGGGTCAACTAAGCCAACGTAGTTACCACAGTCTGATCACCGACCCACTGTGTATCCACATGTGCCTAGATAGTTATGGCTTCTTGGACGCCTTTCTCCATCCGCATCGTGTGAGCCCAACCTCACGTTCATTTCTCTGTTGTTCTTCCGATGTCTCGTCGGAGACGACTGATGCAGAGGGTTTTTTCGGAGGGCAGCTTGGCCGCTGGGGGCCCGCATCTGCTCCGGACGCAGCCTGATCTGCCTAAGATACTTTCTGGTACAAGTAGATTACCACTTCATCAAACGAGCATTTCTCCTTAATCTTTTCGTTTCCAATATTTAAGAGTGCCCGGATAACGCGATAATACGTGAGCAGTTAACTCTATGGCTACGTAGCTCTGCTTCCTACCGCCTCACAATCTGAGGGCATAGGTGTAAATCGGGCCCAGCTCCTGACGTGGCCTGCACTAAGGATGAATTTACGGTGATCGAGCGGACAAAAAGGGTTTAGGTCAACGGGTGCGGGACCACTATTTCGTCAGGACTCTCGCCCGCACATATCCTATCGGACTTGACCGAAACCAAGAAGCCCCATCCAGCTCCAGCGAAAGAAGGTCAGGTGAGCTCATCACCTGTAAACTATCCAAGGAGCCTCAATATGATTGCAGAGTACGGAATATGATTGAATAAGAAGGCGCGTTGCTTTAGGACTGTACCCTTATCCAAGGGAAACACAATCGCGTCTCACAGGAAGGTCCCCGCACCAAAAAGTGTGGCACTCCGTCATGAGACCTGCACTATAACCTAGTAAGCGCCCGATAGTTTAAAGGATGGGACTGTTGTCAGGCCGTGTCCACCATTTGGTGAGGAGTACAAATAGTGTCAGCCATTAGATCAAGGAAACATATTCCATTAAAAACACGAGTGCAGATCCTTATGCCGGACCTCTTCGAGGAGCATGTTAAGTAGCCGAGGCATTGAATGA"
t="CGGGACACCTTTTGCATTCTATTGAACCCAGTCCAGGCGCGTACCCACTATAGATGAAGGTTCCTCTGGCCTCGAGAGCCACCTTCGAGACCTTACGCCATTATTAACGCGTGAGCACAACCAACTCTGGTTATCTCAATTGTTGTACCGAAGTATTGTCGTAGACCCCTCGGCCAAACGCTTGCTGTCTCACGGACCCTGGTAACAAGCGGCCCGCAAGCCTTACGGAGGCTGCCATATCAGCCCAAGCTACTTGCTGTCACTAATACTATAACACTCAAGCCAACGCGAAGCGCTGGGTAACTTTTTTGTTGCTAATGGCCAAGACTGCAGATCGGGCGTAACTATGCATGCCCCGATACCTCTATCGCCAGAAAGCTCTTCTGCATTCCGTAGGCCTATACGCACTCCGCCAGACCAGTCGGACAGCGAACCTGACAACCATGGCTCATATGCAGGTGGAACCATTTACCCGCCGGGAAGATTGAATCTGGTGAGTCAGTAAGTCCTTAGCAAGTCGGGAGGATTCGCAGACTCCTTTATGTTAGCGTACATTACCCACGTAACGGGGTCGCATTGGCCGCGCCCGCATGTGGGATTTATTAATAATCCAATTATAATCTTTTGCCGGAACCCTAATATTACAGCGAATTGCTTTATAGGACCGATATTGCAGCCGATCACGGTGCCTGCCCTAACCTTTCGTACGGCATCCGCGCACACGGAATACAGGAAACCCGCCGCCCTTGAATCTTTGACGCATCGTAATAAGGGATGTAGTTCAAGCTAGTAGACACCTTACACCTCAGCCCCGGGGAATTTTCTCAATCCGGGTATCCAAGATTATCGTCCTGACAAATAGGGTAAGAAGAGGGTTCTACAACAGAGTTTCTCCCAAAACCTGATAGTTAGGTAGGTGTGACTGCTTCCAGCGGAAAGGATGCTAACAAGCACCCGCTATGCCTCA"

hammDist [] [] len = len 
hammDist s@(x:xs) t@(y:ys) len
    | x /= y    = hammDist xs ys (len+1)
    | otherwise = hammDist xs ys len

main = do
    print $ hammDist s t 0