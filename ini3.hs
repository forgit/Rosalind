{-
Given: A string s of length at most 200 letters and four integers a, b, c and d.
Return: The slice of this string from indices a through b and c through d (with space in between), inclusively.
-}

str = "HumptyDumptysatonawallHumptyDumptyhadagreatfallAlltheKingshorsesandalltheKingsmenCouldntputHumptyDumptyinhisplaceagain."
a = 22
b = 27
c = 97
d = 102

slice str a b c d = s
	where 
		(_,stra) = splitAt a str
		(_,strc) = splitAt c str
		s = take (b-a+1) stra ++" "++ take (d-c+1) strc

main = do
	print $ slice str a b c d