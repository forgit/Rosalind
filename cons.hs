{-
Given: A collection of at most 10 DNA strings of equal length 
(at most 1 kbp) in FASTA format.

Return: A consensus string and profile matrix for the collection. 
(If several possible consensus strings exist, 
then you may return any one of them.)
-}

splitStr []     = ""
splitStr [x]    = [x]
splitStr [x,y]  = [x,y]
splitStr (x:y:z:xs)
    | x `elem` ['C','G','T','A'] && y == '\n' && z `elem` ['C','G','T','A'] = splitStr (x:z:xs)
    | otherwise     = [x] ++ splitStr (y:z:xs)

formTuple [] = []
formTuple (x:y:xs) = [(x,y)] ++ formTuple (xs)

removeRosalind [] = []
removeRosalind ((x,y):xs) = [y] ++ removeRosalind xs

makeProfile c a = foldl (addTwoCounts) (head tmp) (tail tmp)
    where tmp = map (countChar c) a

countChar c [] = []
countChar c (s:str)
    | c == s    = [1] ++ countChar c str 
    | otherwise = [0] ++ countChar c str

addTwoCounts [] [] = []
addTwoCounts (a:aa) (b:bb) = [a+b] ++ addTwoCounts aa bb


printElements [] = putStrLn ""
printElements (x:xs) = do 
    putStr (show x++" ")
    printElements xs


formStr c n = [c] ++ ": " ++ (unwords . map show $ n)
outPut c input = formStr c (makeProfile c input)

findMax [] [] [] [] = []
findMax (a:aa) (c:cc) (g:gg) (t:tt)
    | a>=c && a>=g && a>=t     = ['A'] ++ findMax aa cc gg tt
    | c>=a && c>=g && c>=t     = ['C'] ++ findMax aa cc gg tt
    | g>=a && g>=c && g>=t     = ['G'] ++ findMax aa cc gg tt
    | t>=a && t>=c && t>=g     = ['T'] ++ findMax aa cc gg tt
    | otherwise              = "   " ++ findMax aa cc gg tt

main = do
    input <- readFile "rosalind_cons.txt"
    let 
        s   = splitStr input
        tmp = formTuple $ lines s        
        tmp'= removeRosalind tmp
        a   = makeProfile 'A' tmp'
        c   = makeProfile 'C' tmp'
        g   = makeProfile 'G' tmp'
        t   = makeProfile 'T' tmp'
    putStrLn $ findMax a c g t

    putStrLn $ outPut 'A' tmp'
    putStrLn $ outPut 'C' tmp'
    putStrLn $ outPut 'G' tmp'
    putStrLn $ outPut 'T' tmp'

