import Data.Char

conversation input
    | s == "hello"              = "Hello"
    | s == "how do you do?"     = "Great!\r\nAnd "++ s
    | s == "bye"                = "Bye!"
    | otherwise                 = ")))))))))))))))))))))))))))\r\nBye!"
    where s = map toLower input

main = do
    getLine >>= putStrLn . conversation
