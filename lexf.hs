import Data.List
import Control.Monad

main  = do 
    input <- readFile "rosalind_lexf.txt"
    let a = lines input
        t = words $ a!!0
        n = read (a!!1) :: Int
        s = replicateM n t

--    putStrLn $ unlines $ map unwords s
    let aa = input <- getLine >>= return . map toUpper
    putStrLn $ aa
