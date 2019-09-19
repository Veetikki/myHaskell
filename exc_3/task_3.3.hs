--Task 3.3
mostRec :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
mostRec _ _ _ [] = []
mostRec f d z (s:ss)
    | f s z <= d = s : mostRec f d z ss
    | otherwise = mostRec f d z ss

mostList :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
mostList f d z ss = [x | x <- ss, f x z <= d]

mostFoldl :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
mostFoldl f d z ss = foldl (\acc w -> if f w z <= d then acc ++ [w] else acc) [] ss

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
