import Data.List

-- works only on small dimensions
ascend :: [Int] -> [Int]
ascend  = head . sortBy (\x y -> compare (length y) (length x)) .
    filter (\xs -> xs == sort xs) . subsequences

-- works only on small dimensions
descend :: [Int] -> [Int]
descend = head . sortBy (\x y -> compare (length y) (length x)) .
    filter (\xs -> reverse xs == sort xs) . subsequences


{-
    Using algorithm from this video
    http://www.youtube.com/watch?v=4fQJGoeW5VE
-}

-- Longest Increasing Subsequence
l :: [Int] -> ([([Int],Int)],Int)
l d = (([d!!0], 0): 
    zipWith (\(x,a) y -> 
        (
            maxx 
                [if (d!!j) < (d!!(a+1)) 
                    then fst ((fst (l d))!!j) 
                    else [] 
                |j<-[0..(a+1)-1] 
                ] ++ [y], 

            length (takeWhile (/=y) d)
        )
    ) (fst (l d)) (drop 1 d),0)

    where
        maxx = maximumBy (\x y -> compare (length x) (length y))

-- Longest Increasing Subsequence
lis :: [Int] -> [Int]
lis d = fst . maximumBy (\(a,b) (x,y) -> compare (length a) (length x)) . fst $ l d


--Longest Decreasing Subsequence
q :: [Int] -> ([([Int],Int)],Int)
q d = (([d!!0], 0): 
    zipWith (\(x,a) y -> 
        (
            maxx 
                [if (d!!j) > (d!!(a+1)) 
                    then fst ((fst (q d))!!j) 
                    else [] 
                |j<-[0..(a+1)-1] 
                ] ++ [y], 

            length (takeWhile (/=y) d)
        )
    ) (fst (q d)) (drop 1 d),0)

    where
        maxx = maximumBy (\x y -> compare (length x) (length y))

--Longest Decreasing Subsequence
lds :: [Int] -> [Int]
lds d = fst . maximumBy (\(a,b) (x,y) -> compare (length a) (length x)) . fst $ q d



-- TOO SLOW!
main = do
    readFile "rosalind_lgis.txt" >>=
        return . map(\s -> read s :: Int) . words . (!!1) . lines >>= \s -> do
            putStrLn . unwords . map show . lis $ s 
            putStrLn . unwords . map show . lds $ s
