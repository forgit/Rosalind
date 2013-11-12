{-Given: A positive integer n (n≤1000) 
and an adjacency list corresponding 
to a graph on n nodes that contains no cycles.

Return: The minimum number of edges that 
can be added to the graph to produce a tree.
-}

tree (n:s) = n - length s - 1

main = do
    readFile "rosalind_tree.txt" >>=
        print . tree . map (\x -> read x :: Int) . lines