{- 
Given: Two positive integers a and b, each less than 1000.
Return: The integer corresponding to the square of the hypotenuse 
of the right triangle whose legs have lengths a and b.
-}

a = 3
b = 5

ini2 a b = a**2+b**2
ini2' = \a b -> a**2+b**2

main = do
	print $ ini2' a b
