--For each string in ss it computes a list of similar strings in ss (strings that are at most distance d from the string).
findSimilar :: (String -> String -> Float) -> Float -> [String] -> [[String]]
findSimilar f d ss = map (\x -> mostFilter f d x ss) ss

--Task 3.3
mostFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
mostFilter f d z ss = filter (\x -> f z x <= d) ss

--Tasks 3.2
calculateFirstTask :: String -> String -> Float
calculateFirstTask [] [] = 0.0
calculateFirstTask x y = (countTimes x y + countTimes y x)/(fromIntegral(length x) + fromIntegral(length y))

calculateSndTask :: String -> String -> Float
calculateSndTask [] [] = 0.0
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
countTimes x [] = (fromIntegral(length x))
countTimes (x:xs) y
 | x `elem` y = countTimes xs y
 | otherwise = 1.0 + countTimes xs y