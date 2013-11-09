import Data.List

gen 0 _ = [[]]
gen n xs = concat ([""]:map (\x-> map (x:) t) xs)
  where t = gen (n-1) xs

printList = putStrLn . unlines . tail . format
    where
        format [] = []
        format (x:xs)
            | ' ' `elem` x  = format xs
            | otherwise     = [x] ++ format xs

format' acc [] = putStrLn . unlines . tail . reverse $ acc
format' acc (x:xs)
    | ' ' `elem` x  = format' acc xs
    | otherwise     = format' (x:acc) xs


main = do
    input_raw <- readFile "rosalind_lexv.txt"
    let input = lines input_raw
        s = input!!0
        n = input!!1
    format' [] (gen (read n) s)


