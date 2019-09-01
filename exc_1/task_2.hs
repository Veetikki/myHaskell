--Function that, given a list of strings and a character evaluates to a list with all the strings of the input list that either begin or end with the input character.
stringWithChar :: [String] -> Char -> [String]
stringWithChar [] x = []
stringWithChar [y] x = [y | last y == x || head y == x]
stringWithChar (y : sy) x = (stringWithChar [y] x) ++ (stringWithChar sy x)