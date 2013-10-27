{-
Given: An RNA string s corresponding to a strand of mRNA 
(of length at most 10 kbp).
Return: The protein string encoded by s.
-}

makeTuple [] = []
makeTuple (x:y:xs) = [(x,y)] ++ makeTuple xs

s = "AUGUGUAUACAUUGCUCGCGCAGGUCGCAGAGACCCCUAACUAAUCGCAAGUUGGAAUGUCUCUACAGGAGCCCUGUCGAGCAUCUAAAUGCUCUUGGUGAUCGCGGUGACAAGCAUCGACACACAGUCUACCGUUGGCAUAAUCUAGGCGACCCAUGUCGGGAUCAAUCGUUCCAGUCACCCUCACCCUUACAUGCCAAACACCGAAUUUUCUCUACUCGUUGGAAAGUGCUCAUUAUCACUCACACCCCACUAUCCGGUAUAUCCGCGCUAAUCCCUCAAAGUAUCAUCAAUGCUGCUAACCUUUCAUACGUAACUGGCUACGAGUCAACUAUCACUCCGAGUCGAUGUGGAUCGGCUGCGUUUGGAGUACAACAAAGGCUCACCGGAGAUACGGUCGCGCCACGUGUCUGUCAGCCUGACGGCUCGGUGAGACUCUUGAACGAACUAUGGCAUAAGAUGGGACACGACUUUCUUAACGCUCAUCGCCUUUCCUCAGUCCAUCAGGACCGUAAGGGGCGAGUCAGACGGUUUAUGCUCUGUUGUUUGCGCACACUCACUUGCACUUUAAUUCAAGCUGUAGUCAUGGCAAGGCAGCUUUUCGACCAUAGGAACGAGGGGCACGCGCCGGAUCUGAGAUGCCCGCCAGGUUUCCUGUACGCCACCGUCGGGCACUCUCUAUACAAAGUAAGAGCGAAUGCCUAUCUCUGGACCAUACGCGUCCACUCUGACGAGGUUGGAUUCAUGUCGAAACUGUCAACCGCAUGCAGGCGCCCAACUUAUCCUUGGCCAAUCAUUUAUCUGCUAGGGAAUUUCGCAACUAAUAACGCCUACUGCUCCCAUAACUCAGGUCCUAGUACCAAGUCAUCCGAACGCCUCCGUGGACGGUACUCGCAUUGCCCCUAUGGACGCUUAAAUUCCCCGACGGUCUAUGGCUACGAUUCUACGUGGAGCAGACAGCCUGAUAAAUCCACGGAACCUGACUAUAAUUGGAGUUUCAUUCUCAAUCACUGUGGUUGCGUGGGGGCCGUCGGCGGUCGUAGCGAAGGCCCCGAUAAUCGUUCGAUGGUCCAAGUAUCCGUGUCUGCUCAAUGUGGGUGCCACUGUAUAAUUGAGAAAGUGGUGGCUGCUUGCCGGCCACGUCGGCUAGUACAUAUUAAUGUUCGCGACCUAUCAUCGAGGACCCCGCUCGAGGGGGCUUUGCGAGCAGCAUGUCGUGGGGUUACGACCGUCAGUGGUUACUUCUACACACUGAUCCGGCGAGGACUCCCACAACAAACUACCCGCUCAAUAAAGAACUCGGGAUGGAGGUCAUCUGAUGUGGAUACUUCACAUAUAGAGCAACAUCCUCGGUCGAGCACAAGGGACAAGUCUGGGGAAGAGCUAUCCAGGGUUGUUGUCCCUAGAUCGCGACUCGAGGCGCAAAUCUCGCAUCGCGCCUCCGGCAGCUUUCCGGAUACGGCAGCAGACGCCGUUCCAACCACUGACGUCCUAGCUAUGAAGGGAGCACACGUUGCGCGCGCUUACAUGCUAAAAUCCAUACCACGACAUCUGAGAUGGCAUCAGCGAAUGGCCCGGUCCAUGGGAGCUGAGUUAGCUGGACCUUACAUGUUUACUCCUCCCUGGAUAAGUCUAUUCUGGACUAUUUGUAUGGUUUUCUACCAACUGCCCUGCCGUUCAAGUGUUCCGGCUCUAUUCGGCGAUGAACAAGUCGCCAGAUGUAAGCAUGAGCUGUUCACAGUUUGUGAUGAGACACGGCCAGCUAAUCGUGUUUGUGUUAGUCUGGGCGGCGUUAGGGCCGAAAGUGGACGAAAAUGCUGUAUUUUUUUGCGUCACACUGGUUAUAAGAAGGGCCUCCCCGUGGAGAAGGGAAUCUCUCAUUCAGUCGCUUUACAAUGCCUACUCUUAGAACGCUUAGACCUGUCCGCAACGACGACCCAUCGCGCUAGGUCCUCAUUAUUAGCAUUAAAUCGUCAACUACACUAUUGGUCGCCUAUACACCCUGCUGCCGAGUAUUUUGGCGUACCUAAGGAGCCGGACCUUACCCGGGGAAGUAUCGUAACGUUUCUUAAAAGCUCGAAGGUUAGACUUAGAGUAUGUCAGAGAAGGAUCUUUCUAGACGUGGGCCAAAAGUACAUAAUAGUAGGCAUAAUAUGUAUCGCACAAAACUUAUCCCCAUUCUCGCGCGAUUGUGCGGCGCGGGUAGUCGGUCUCACCCUAUUACCAGAAAGGGGGGGUCCCGAUCCUACUACUCACGCCUGUCUGUCGGGAACGUCUUACGUGAAUGUCUCAUGGUUACGAAAUGAUGUCUUCCCAUCCGCGGCUUGUCUUCACUUUACACGGAUUCGAGCCUCCAAUUGGUGUACGACUCUCUCAGACCCGUGCCUAGUGCGACAGGACGUUAACCGAGAGAGAAUUAUCAUGGUCCGGCUAUUACACUUAUCUCGGAUUUCCCCGCAGUUACGGAUCGUCGCUGACUGCACAUAUUGUUUUGUACCUUCACGUCAGGCUUCUAUACGAGAGUGGAGAGAAUCGGUGUGGCCAACCGCUGUGAUGCCCCUGGACCAGUCCUUUGAAACUCCGCCUAUGAUACCGAGGGCUUUGUGCGUGACGCUCGACGAGGUCCGAAUAAACAGUAACUCGAUAGACGUGGUAUAUCUGGAACGACGAGACCUAUGCAUGUAUUCACAAGGUACCGAAGGCAUAAUCUGCCUUGCAUCUCGGUACAGGAAGUUUCCAUCCACUAAUAACAUAGGGGGACGAAGACUGCGAUGCCCCAACGGGGAACAGCGCAAGGUUUACAAUAAAUUCCCUACAAGGUUUAACAUCUGGCAAACUCCCAAGCGCUGGAGAUUCCUGAUACGACGACGUGGAGAUAUUCGGUCCUAUGACCACAGAUGCAUAUACUCGAAAAGGCCUCAUCUUACCAAUCCAUUCAAUUCGGGAACGCUACAGAUGAUUAAAUUUCUCGUCGCAAUCCUCGUUCCCGUUCGGCAGAACUGCAGCCUUCUAGACCUCCACGACCUGCGCUGCGUCCAGGAGCGUUCUUCCACCUUAGUGUUUCUUUAUAUAAUUCAACUGGAUAUUCGCUAUAGUUGUCCACAGGAGCCUCGUUCUCUGCUACUAAAUUCUCAAAAAAGGGACUACAUUACCUAUCGUCUUAAUACGACAAAUUGCAGCUCCCCGCGACGUAUCGGCAGAUGGGGCUUCCUACAUACUAUUCUCCUUGGGUUCGCUGCAUCACGUUCGGCACACCAGAACUUAAGAAUAAGUCUUAGAUUAACCUCCUUUUCUGUCGGUAGGGCAGGUUUGGGCACGUCUUUAGGCCUUCGUCACCCUCUCCAAACCAUGAAUUCCUUCCUGUUGCUUAUGAUUCCAUGUCUGCGACAAGGUCAUCCACCAUCCCACGCCUGGUUGGGGGGGUUGAUAGCUAGUGAAGCUACACUUCAAUGCAGGUUCAGCCGGGAUGCUGGGCGAAUAUCAAAGGAAAAGGCCCAAAGGACGGGCCGUAGUCGUGUGCUGUUAGACCCAUUUAUAACAAGGGGCUGCAAUGUAGAGACUGUUGAAAAAUUCAACAUAAGUCGACUAUCCUCGAAACCGCAGGAAUGGAAUCGGUUGCUGCAUCACUUAAGAUGGGUUAUGUCUCAAGGACAGCCCGAAGGUGGUAGCAAGAAGCCCCUUCAGGAAUCCGCAGCAAGGGGCAGCCGACGUGCUCAACCUCUGGUUGUGAAGACCCGCAAAGGCGCUGACCCAGAGUGGGUGCUGUCAAUUCGUGACCGGCUGGAGUCACUCAGUGAGCAAUUUCCCUUGUUGCCGUGGAGGGCCGAUAUAUUACUCCGCAAGCUAAGAGGGUUAUCGUUCCCCAUCACGGCUGUUUAUAGAUGUUUGCUAGAAUCCCCUAGUAUCAUGAGUUGCCUUUGCUCCCGUUCACCAGAGGAGGGGGACAGCUCUCCGUACUUCCGCCCUUGCCGCAGACCACGCGUCCCAGUAGAUUUCCUCCGGGAUGUCGCGAACAGCCAGCCUAGCACACUGACCGCAGCUCCUCAAGAAGCGUGGCUGGGUACCUCAUAUUGUACCUUCGCUGUUUUGGCGUCGGUAUCUCUCUGCUCCCUCGAUCGGCGUCCUUACCGGGCAGACGAGCACCGUGUUUAUUGGGGCUCUGCGUACAACAGAGGUAUAGCUAUUCCCAAGGAGCAGCACGAUAUUGUGGACCAUAUAAUGUCAAAUACGGUACAGAAGACCCGGCUACCGUGGGUCAAGCGUAAUUGCUUAUUUCAGAGGCCAAUACGAAGGCUAUUCGUUCAAGGUAUCCCGGCUCCAAGGCGUGGCGGUCAGGCGCCCCCGCGUUAUUUGAUACAUCGGGACCAUUUCGCUGUGAAUCGUCCCGAGAUACGUACGUUUGCAAGACAGUCACCUAGUCGAGGACUCUAUGAUGCUGGUACAUCGACCGUCUCAAUUGCAGACGCGGAGGACAAACAGCAACCCACCCUUAAAACCCUUCAUAGUGAGCAUUUUAGUCAAGAUUCCUUCAACAGUACGGUGGGUGGCGUCUGUCCUGCCGAAUCAACCGCCAACCAGAAACUGACCCGCGGCACAACCUAUAAGAUCGCAGAUUUUCGCGCCCUGGUAGCUAUCGGUGUGAAGUCGACAGGCAGGUGCGGCUAUUUUCCCCUUGUACGCCCCAGUAGGAGUCCUAAACUAGCACGCUAUGUGUGUCAUCAGGGAGCUCAACCUUGCCAUGAGGUAUCGCCAGAGGGGAGCUCAGUGUUGGGGGGCCAUAAAAACCUCGUGCAUAGAAACGUCCUGUUCACUUUGUCCUUUCGGACGGGGUAUGGACAUGUUGUUGGCGACGGCUUGUAUUGCUCCACUGUCAACGCAAGAACAGAGAGAGUGGGAUUUAGGCACGACGGUCCUGACUUGCUGCUAAUCGCGUCGAGUGCGAGUAAACACGAUUUACAGGUCUGUCAGUUGAAGCCUCUCUUUCCUCCAGCAUACCUAGUAGCCUGGGAACUCGAGCUCCACCUGUUUUACGCGCGAACGACCACGUAUAUGGCUCUCUGUUCAGAGGGCCCCCCGAGACAACCGUGUCAGGCUAGGAGUCUAAGCAACGUUCCUAUAAUCCUGGUCCGUGGCGGCUCUCGCGAGAAUCUACUUAACAUGAAACUAACCAGUCUUUUAUUCUGUCUCCGCUCGGAGUAUUCGAGAGGAGUGACCAGAUCGUGGCGAGUGAUGUGUGAGGAGCAAUCAAUGGCUAAAGAUGGAGGACUCGCGCGGCCACAGUACUUAUCCUUUUUCGGUGUUACGGCGGCGGUAUUGACCGCGCCGUCACAGAUGAGACUUGCAAAAAACCGGAACAAAGUCACCACUACCGUCUUUGCGGACGGGUUCGUUGGGGCCUUUCCUCAAACGUUUCGAAUCAUCGGAGCCGAGUGGGGGUCACUAGAACCUGCGCUGGUCUUGAACCGAUGUACUUAUCCUAUUCUGAUGCGUGAUGCAGGUGGUUGUUUAUUCCGGCUGAAGGUCGAAUUAAUCUGGUUAAGUACCCAGAACCUGGGACAUACCGUCCCGUGGGAUGUGUCGAGCUUAGGUACCAGGACGCGCAGAGGUUCGUGUGGCCAUUCGAAGAUCCAUUGCUUGCCCGCGCCGAACGGCCCUGAUGUAGGCCCGUUGGAAGCUCGGAACCACAGCUCGCUCCAAAAGCCCCGACUCCAUCUAUUCAACGGUCAGAGCCUGAGCUCUGUAAUAAAAAGGAGCAUACCCUGUAUAGACCUCGCGCUAGACAAGUUUUCCCCCCGCCUCACGCCGCACGCGGAACAUGGUCGUUUUUUUUUCUUAGAGCUAGAUCUGGGUUGUCGUUUUGCUUGUCGACGUCGAGCGACCGUAGUUACCCGUUCUAUCACUUAUACAUUAACUCACGGAAGCAGCCGUGCUAGGAUUAAUCCUGGUGUGAGGACGGUUUUAUGGAUACCGCACCUCUCGGUUGAGCUAUUCCCGCGUCUUAUGCGGGAGAAGCGCAGACCCCGGCGCUCGCAACAUCGCGCUCUCCACGCUAUGUUAGCGCCGUUUACAACUACGUGUUCUACGCUCCCGUUCCUAAGCAGCGCCGGAGCGCAAGGCCUCGGCCCCCGGUGCCCGGGUAAUAAGGUGGACUUCGCCCCAACUUUCAACCGCCGAGUAGACGAAAGAAAUGUUCGACGCACUAACGGGGAUUGUGUCGUCAGCGUAGAUGGAACGGACUGGAUUUUAUGUGUCCGCGGCGCUGUUCGUGGGGAUGUCAGGGCCGCGCUGGUAGAAUUGUCACACCUUAGCUCCCUACGAAGGUCGGACCUGAGCAUUUUAUACCUGCUUCCCGACCUGCUAGGACCAAUAAAUCCACUGUAUACCCUCUCGAUUCUGAUGUACGUAGGAUGCAAGCGUCUUCUGAUAUAUGGCGUUCGGAAGGAUAACCCCCACCAACCCGCUGUUCGUUCUCGUCCACGCGGACGACUGGCAUUCCGCCGGAGACGAAACCGUGAACUCUCUACCAUCGACUAUUAUCAGCGCUGGAAACAGCAAGCAUGGAUUCGGCCGUCGAAUCUGGGCGCCUGGCCCAUGUAUAGGAUCACGACCAGUCUCGGCCAACACGAAGGCGUCAGUCAUGUGUUAAGACCAGAAAGGGGUCGUCAGCGCUGGGCGCGUGCGAUGGCCAUUUUUGGUGUACUACUCAGUCAAGUCAACUAUCGGAACUUUGUCCCAGUUCGCAUCACUUUGCGCAAGAUCGACCCCGACCGGUCGGUGCCACCCGAAGACCGAGCAUACACGAUGCUCCAAAGAAGGAUUUUCAAGAUAUAUGAUAACAUAAAUAACACUGCACCACUGUCGCCGAGCGUAACUAAUGUUCGGCCGCUGAUCCGAGGGAUAACGGCGUUGAAGAAUCGUGGUCUUUACGUGUUAACGAUGCCUAAAAAUCUCUGCAUGACCACCGGGGAUCUGUGUAAAUCGCGGCAGCACUUCGGGGUUUCAAUAUUAACGGACGGCGGAACCCCCCCUACGGCUUGUAGAGUUAUCCACGACUCUCGAGGCCGGCAAUUGGUGUACAUUAAGCCUCUGAUCAUGGCCAGUCGACACCACGCCGUGAGCGAACAUCUUAUGGGGUUAAUUCAAUUUAGUGGUACCUACAGCCGCCCUUUAUAUCUAUGUCAUCAGACUAUUAUCUGUUUGCAGACCCUGACUGUGCGAAUCGCUGAUUCGCCUCCGUACUGGUGGUAUACCGCAGAAACCCACCUCCGGCAGUGGAGGUUGUUGCGCCGAAUUACCCCGUGUGAUCUUCAGGACCCAUUCGAACCAUUCCCCAGUCACGCGACCCCGGAGCCUAAAUUCCCAUUGAGAAAAUGUCUCGCUAUGUCACGAAUCCGUUACCCGUCUUUCACAUACACGUUGCACAUACGCGCCACCAAUUUACUCACGCCGGCUCUCAUGCCGGCAAAUCAGUACUGUGCGCCAUUAGACUGGCAACAAGCUGAGUUUGUGAACUCACAUACAUACCUGAUAAGGUCGCGUAGUGACACUGAGUAUGGCCGCAGAGAGACCGCAGAUACCUCCCUAGGGGUAGGCCUGCUGCCCAGCCGGCCGUGUUAUAGUAAGUCGAAUCAACCGCAGCCUCUACAGGAGACACGAACAGACGCAUUUGCCCCUAAUGUCUGGAGCGAAGCUGCUGCUUCUUGUGGCUUUAUCCGGUCAGCCUAUGUAGCACUGACUACUUGGCAGUAUAUCGGCAUGUGUGACCGCAAAUGCCAUUUAUUGGAGUUGCUGAAGUGGGCGAGUAAGAUUAUUUUGCCGUCAGUCCGUAUGCUACUAAAUAUAUGUAUGAUGGAAAUAGAUGUUCGUGCAUAUCAACAAAGUGUGGCCCCUUGGAUUCUGCACAUUAGGAAGCUACCGACUGGUGGACAAAAAAAUUCCAGCAAAACUACCAAAGAGACAGGGGGUUAUAACCGAUGGAGGGGUGGCGACGCUCGUGGUGGGUCCAAGAGUAAGUCGCUAAACUUUAUGACACCCAGGCCUAGCGCGGCGGCGGCUGUAUAUGGGAACAUAACCAAGGUCGUGCACUGCCACAUGUAUAGCGCGCCGCUCGCUCUACUUGAAGACGAUUGCCCGCCUCUGCCUGUAGCAAAUUACAAUUUUUAUAAGCCAUCAUCCCGGAUAACAGGGAUAAUUCUAUCGCACUUAAAACAAAGGAUUGUAGUACUAACGACCGCGCCCAUGGAAGAGGCGCGACCCACCACCACCUCUGGCUGUUCUAGGCGCGCUUCUUAUCGGUGUAUGUGUGUAAGAUCUCAGACGGUUCGCUCCAACAGUCGCUGGGUAUACCACGUCCGGAGAACUAACAGUUGUUGGGGUAUUAUAGGACUCGGUGUGCAACGGGCAGCGAUUUAUCGUUCGAAAUAUUGUGUAGAGAUCUCUAGGAACCCCCCAUUAGACUCGGGCUCAAUAUCGUUCCGGUAUGCGCUUUCUUUUAAUGCCUGUCCUAGAGGUGCGGCUAUAGGAUGGGGCUGGGGAUCGACGUUAACUGGGAUAGCUACCGAUCUUUGGCGAAUGAUUCCUCUCACGGGACAAGUCUUCCGUCUUAGGAGUGGUGAGCAGGACAGGAUACUACUGCACCCAAAGCAAUACGUUGGUUCGCGUACACAAUUGAGAUCGCAGCUUGGCUUCGGUCACAAGAAGGUGCCCAAGACGUGUAUGUGCUUCACGGGCGGUACAUCGAACAAUCUGGAUGUUACUGCCCGGGUAUGUAUUUUGAUGAAUCCACUCAGUGAAGCUUCGGCGCACUGCAAGUGCAACUGGGUCACGCGGGAGCUGUUCUUCCUGACAAGAGGUUCGAAAAGUAAAGUGCGAAAAGGGUUCGUAUGCAUAAACAAAUUGUUCAGCACAUUUACCCUUAGGGCUUGGUGUUCUAACAACCAAAAGGUAGAGCAUCCUUCUAAUGAAUCUCUCAUUGAUCCCCGUUAUUGGCAGCGUGGGUGCUACUCCACUUUCAGCCUCAUUGGGUCGUCGCAUCGUUUGUAUUCUGGUGGAUCGCUCCGCGCCGCCCCCAAUGGAGAUACAGCGCGUAAGACAGUGAUUUACUCGGAGAACCGUGUACCCGAGUUCCUUAGCCCUUCAAUAAAAUCACUUAUGGCAUGCCUUUCGCCCUCCGCCUGUAGGGCGCUAUCUGACUCGAAUGAGCAUUGGGGGUUACCUAUAUACUCUCUCGCUUCGAUCUACUUCCAACCAACGGGUCCACUCAGGGGUCAACGAAUGGGUUCCCUGCACGCUGUUAUGGGCAACUCUCGGGCCAUCGGAGGGGAACGAUGCCCGAGAGGGUUUGCCCCUAUGAAUUUGGGGAACAAGGGACGUGCCAAUAGCCUGACCGCGUGUUUUCGACGUAUGGGAAACCGCCUAACCACGCGCCAACCUACCAAGUGUCUCGGGGUGGGUCGAUCAUGCGGAAACUGCCAGUUAUGCUACAACUGCUUCAAUGACCGGGUCUCAGCAGACCGCUAUUACAACGGCAGUGAGUUGCUUAACCUGGUCUUGUUCUACAGGAAUAUAGGCCCGGAUACUGGUGAAAUUGCAAGUUAG"
s' = "AUGGCC"

searchTable str []  = error "Mismatch"
searchTable str ((w,k):table)
    | str == w && k /= "Stop"        = k
    | str == w && k == "Stop"       = ""
    | otherwise                     = searchTable str table

proteinStr [] table = []
proteinStr (x:y:z:xs) table
    | otherwise = searchTable ([x] ++ [y] ++ [z]) table ++ proteinStr xs table


main = do
    input_table <- readFile "RNA_table.txt"
    let 
        table = makeTuple $ words input_table
    putStrLn $ proteinStr s table
