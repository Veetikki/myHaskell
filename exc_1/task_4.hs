--function that, given a list of numbers, produces a list with all elements of the input list such that the element is followed by a greater number in the input list
followerGreater :: [Int] -> [Int]
followerGreater [] = []
followerGreater [x] = []
followerGreater (x : xs) = [x | x < head(xs)] ++ followerGreater(xs)