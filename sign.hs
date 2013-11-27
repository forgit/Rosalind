import Data.List

permute [] = [[]]
permute xs = concat $ map (\x-> map (x:) (permute (delete x xs))) xs

main = do
--    print $ length [(x,y) | x<-[1,2,-1,-2], y<-[-1,-2,1,2], x/=y, x/=(-y)]
    let n = 3
    let t = map (\xs -> map (negate) xs) $ permute [1..n]
    let a = permute [1..n]
--    print $ concat $ [[x,y]| x<-t,y<-a, (abs x)/=(abs y)]
    print $ concat $ [[x,y]| x<-t,y<-a, x/=(y)] -- bad