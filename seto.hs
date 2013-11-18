import Data.String.Utils (split)
import Data.List (intercalate, union, intersect, (\\))

main = do
    a <- return . lines =<< readFile "rosalind_seto.txt" 

    let a1 = split ", " . tail . init $ a!!1
        a2 = split ", " . tail . init $ a!!2
        u  = map show [1..(read $ a!!0)]

    mapM_ (\s -> putStrLn $ "{" ++ intercalate ", " s ++ "}") 
        [union a1 a2, intersect a1 a2, a1 \\ a2, a2 \\ a1, u \\ a1, u \\ a2]
