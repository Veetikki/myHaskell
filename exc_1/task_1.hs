--Points which Manhattan distance from origin is same
--Clearly pairs can be only in range -x .. x
manhattanDistance :: Int -> [(Int, Int)]
manhattanDistance x = [(y,z)| y <- [-x..x] , z <- [-x..x], abs y + abs z == x]