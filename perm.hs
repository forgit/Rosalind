import Data.List

perm n = [1..n]

--swaps element at position a with element at position b.
swap xs a b
    | a > b = swap xs b a
    | a == b = xs
    | a < b = 
        let
            (p1,p2) = splitAt a xs
            (p3,p4) = splitAt (b-a-1) (tail p2)
        in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)


cartProd xs ys = [(x,y) | x <- xs, y <- ys]
cp xs = [[x]++(filter (/=x) xs)|x<-xs]++[[x]++(filter (/=x) (reverse xs))|x<-xs]

aa [] = []
aa (x:xs) = [[x] ++ (filter (/=x) xs)] ++ aa xs

p [] = []
p (x:xs) = do
    p xs ++ [x]


xor a b = (a+b) `mod` 2

del (x:xs) = xs++[x]

main = do
    print $ [1..5]
    --print $ swap2 [1..5] 2 3
    let
        i = 2
        j = 4
        xx = [1..5]
        (xs,(y:ys))=splitAt (i-1) xx
        (ts,(z:zs))=splitAt (j-1) xx
    --print $ xs ++ [z] ++ ys
    print $  cartProd [1..3] [1..3]
    print $ aa [1..4]
    print $ swap [1..4] 2 3
