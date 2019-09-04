--Write a function that, given a string, validates the string as a Finnish IBAN code.

changeLetter :: String -> String
changeLetter (f:i:xs) = if f == 'F' && i == 'I' && length xs == 16 then 
	let  f = 15
	let i = 18 
	let newStr = (drop 2 xs) ++ (f ++ i ++ take 2 xs) in newStr `mod` 97 == 1 else False
