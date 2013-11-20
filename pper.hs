--factorial :: Integer -> Integer
factorial n = product [1..n] `mod` 1000000

main = do

    print $ (fromIntegral $ factorial 21) / fromIntegral ((factorial 7) * (factorial 14))