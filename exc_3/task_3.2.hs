--Tasks
calculateFirstTask :: String -> String -> Float
calculateFirstTask [] _ = 0.0
calculateFirstTask _ [] = 0.0
calculateFirstTask x y = (countTimes x y + countTimes y x)/(fromIntegral(length x) + fromIntegral(length y))

calculateSndTask :: String -> String -> Float
calculateSndTask [] _ = 0.0
calculateSndTask _ [] = 0.0
calculateSndTask x y = (countChars x + countChars y)/(fromIntegral(length x) + fromIntegral(length y))

--This function counts chars 
countChars :: String -> Float
countChars [] = 0.0
countChars (x : xs)
 |  x `elem` ['1'..'9'] = countChars xs
 | otherwise = 1.0 + countChars xs

--This function counts how many times first string chars appear in second one
countTimes :: String -> String -> Float
countTimes [] _ = 0.0
countTimes _ [] = 0.0
countTimes (x:xs) y
 | x `elem` y = countTimes xs y
 | otherwise = 1.0 + countTimes xs y
