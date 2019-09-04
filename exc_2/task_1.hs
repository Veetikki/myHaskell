--Write a function that, given a string, uses recursion on the list of characters and checks whether the string contains only digits or not. Empty string should return false.

checkDigits :: String -> Bool
checkDigits "" = False
checkDigits [x] = checkNumber x
checkDigits (x:xs) = checkNumber x && checkDigits xs

checkNumber :: Char -> Bool
checkNumber x = x `elem`['0'..'9']
