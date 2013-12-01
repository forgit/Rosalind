import Data.List

a = "ATTAGACCTG"
b = "CCTGCCGGAA"
c = "AGACCTGCCG"
d = "GCCGGAATAC"


main = do
    --print $ union d $ union c $ union a b
    print $ length $union a b
    print $ length a