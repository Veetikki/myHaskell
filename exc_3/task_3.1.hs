computeChars :: String
computeChars  = "Odds: " ++ computeOdds (zip ['a'..'z'] [1..]) ++ ", Products: " ++ computeProducts (zip ['a'..'z'] [1..])

--this function calculates odd numbers
computeOdds :: [(Char,Int)] -> String
computeOdds [] = []
computeOdds (s:ss)
 | (snd s `mod` 2) == 1 = fst s : computeOdds ss
 | otherwise = computeOdds ss

--This fucntion calculates char that are two odd integer product greater that 1 and greater
computeProducts :: [(Char, Int)] -> String
computeProducts [] = []
computeProducts (s:ss)
 | snd s < 4 = computeProducts ss
 | length [x | x <- [2..((snd s) - 1)], y <- [2..snd(s)-1], snd s ==  x*y, x `mod` 2 == 1, y `mod` 2 == 1] == 0  = computeProducts ss
 | otherwise = fst s : computeProducts ss
