--Write a function that, given two strings s1 and s2, computes a common "substring" of s1 and s2
findCommonSubString :: String -> String -> String
--if other one is empty we return empty string
findCommonSubString _ [] = []
findCommonSubString [] _  = []
findCommonSubString (s1:ss1) (s2:ss2)
    --We first check if first elemt apears in other string if it does then we remove that char from both string and all other before them
    | s2 `elem` (s1:ss1) = [s2] ++ findCommonSubString (drop ((elemIndex (s1:ss1) s2)) ss1) ss2 
    | s1 `elem` (s2:ss2) = [s1] ++ findCommonSubString ss1 (drop ((elemIndex (s2:ss2) s1)) ss2)
    | otherwise = findCommonSubString ss1 ss2
    where 
        elemIndex :: String -> Char -> Int
        elemIndex [] _ = 0
        elemIndex [x] y = 0
        elemIndex (s:ss) c
            | s /= c = 1 + elemIndex ss c
            | otherwise = 0
