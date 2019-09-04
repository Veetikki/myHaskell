--Write a function that, given a pair (c1,c2) a gap g, and a string s, returns an Int telling how many times (c1,c2) appear in s with gap g.
findGaps :: (Char, Char) -> Int -> String -> Int
findGaps p g [] = 0
findGaps p g [c] = 0
findGaps p g (s:ss)
    --we need to check right values if g is bigger than last part or equal of string length then it can't contain gap
    | g >= (length ss) || 0 > g = 0 
    -- First we check that s == c1 and then we take g + 1 elements of list so gap + last and check if last is c2
    | (s == ( fst p )) = if ( last $ take (g + 1) ss ) == snd p then 1 + findGaps p g ss else findGaps p g ss 
    --if first wasn't c1 then we check last part of string
    | otherwise = findGaps p g ss