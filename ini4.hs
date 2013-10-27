{-
Given: Two positive integers a and b (a<b<10000).
Return: The sum of all odd integers from a through b, inclusively.
-}

a = 100
b = 200

sumOdd a b = sum $ filter odd [a..b]

main = do
	print $ sumOdd a b
