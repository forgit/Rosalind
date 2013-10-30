import Data.List

perm n = [1..n]


swap2 xx i j = xs ++ [z] ++ ys ++ [y] ++ zs
    where
        (xs,(y:ys))=splitAt i xx
        (ts,(z:zs))=splitAt (j-1) xx

--let (ys,zs) = splitAt n xs   in   ys ++ [new_element] ++ zs
--let (ys,zs) = splitAt n xs   in   ys ++ (tail zs)


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
    print $ insert 3 [4,2,1]
