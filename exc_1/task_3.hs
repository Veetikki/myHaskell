--Card game, I assumed that player can only get points from highest possible condition, e.g. checkPoints ('s', 14) ('s', 13) will return 14
checkPoints :: (Char , Int ) -> (Char , Int ) -> Int
checkPoints (c1, n1) (c2, n2)
    | ((c1 == 's' && n1 == 14) || (c2 == 's' && n2 == 14)) = 14
    | ((c1 == c2) && (abs(n1-n2) == 1)) = 8
    | (n1 == n2) = 6
    | (abs(n1 - n2) == 1) = 4
    | (c1 == c2) = 2
    | otherwise = 0