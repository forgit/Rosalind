import Data.List (delete)

-- adorable solution by Mgccl
permute [] = [[]]
permute xs = concat $ map (\x-> map (x:) (permute (delete x xs))) xs

printColumn [] = putStrLn ""
printColumn (x:xs) = do
    putStrLn $ unwords $ map show x
    printColumn xs


n = 4    

main = do
    let l = [1..n]
    let p = permute l
    print $ product l
    printColumn p

