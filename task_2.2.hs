--Write a function that, given a string, validates the string as a Finnish IBAN code.
--You can test with "FI4250001510000023" and "FI3250004699350600"
validateIBAN :: String -> Bool
validateIBAN x = let rx = removeSpaces(x) 
    in if length rx == 18 && tail(take 2 rx) == ['I'] && head rx == 'F' 
        then let newX = changeLetters(moveToBack(rx)) 
        in (if checkIfDigit (newX) == True 
            then ((read newX :: Integer) `mod` 97) == 1 
            else False) 
        else False
 
--First function that removes spaces if IBAN code is for example FI42 5000 1510 0000 23 then FI4250001510000023
--this was not needed but did it anyway
removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces [' '] = []
removeSpaces [x] = [x]
removeSpaces (x:xs)
    | x == ' ' = [] ++ removeSpaces xs
    | otherwise = [x] ++ removeSpaces xs

--Then we need to move 4 first letter to the back
moveToBack :: String -> String
moveToBack x
    | length x > 4 = drop 4 x ++ take 4 x
    | otherwise = x

--This function changes F to 15 and I to 18, we only need to check these since we are checking format anyway
changeLetters :: String -> String
changeLetters [] = []
changeLetters [x]
    | x == 'F' = "15"
    | x == 'I' = "18"
    | otherwise = [x]
changeLetters (x:xs) = (changeLetters [x]) ++ (changeLetters xs)


--Let see after changes that is right type
checkIfDigit :: String -> Bool
checkIfDigit "" = False
checkIfDigit [x] = checkIfNumber x
checkIfDigit (x:xs) = checkIfNumber x && checkIfDigit xs

checkIfNumber :: Char -> Bool
checkIfNumber x = x `elem`['0'..'9']