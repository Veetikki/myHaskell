{--
6.3 Make a program that reads repeatedly lines from terminal (that is a program you need to compile). If the line is follows the format of one of the three lines below:
Int ‘+’ Int
Int ‘-‘ Int
Int ‘*’ Int
then calculate the result of the arithmetic operation. Otherwise output an error message, like “I cannot calculate that” or something. Stop when the user types “quit”
You can use the prompt you like. putStr prints a string without the line change. A possible execution:
calc> 3 + 5
8
calc> a + 3
I cannot calculate that
calc> 4 - 3
1
calc> quit
bye
--}

import Control.Monad

main = do
    putStr("calculate> ")
    line <- getLine
    askForCalculate line
    putStrLn("bye!")

askForCalculate :: String -> IO ()
askForCalculate line = do
    when (line /= "quit") $ do 
        checkInput line
        putStr("calculate> ")
        newLine <- getLine
        askForCalculate newLine

checkInput :: String -> IO ()
checkInput "" = putStr("")
checkInput inputString = 
    let parse = parseInput inputString 
    in case parse of
        Nothing -> putStrLn("I cannot calculate that!")
        Just ( ( x, y),'+') -> let sum = x + y in putStrLn (show(sum))
        Just ( ( x, y),'-') -> let min = x - y in putStrLn (show(min))
        Just ( ( x, y),'*') -> let tim = x * y in putStrLn (show(tim))
        

parseInput :: String -> Maybe ((Integer, Integer), Char)
parseInput inputStr = 
    let noSpaces = [c | c <- inputStr , c /= ' ']
        fstInt = reads noSpaces :: [(Integer, String)] --We first check that first is int
        symbol = [c | c <- noSpaces, c == '+' || c == '-' || c == '*'] --we check that right amount of symbols and right type
    in if (fstInt /= [] && (length symbol == 1))
        then let sndInt = reads (drop 1 (snd (fstInt!!0))) :: [(Integer, String)] --we check that last one is int
            in if(sndInt == [])
                then Nothing
                else 
                    let fixed = "(" ++ map (\x -> if x == symbol!!0 then ',' else x) noSpaces ++ ")" in Just (read fixed ::(Integer,Integer), symbol!!0)
        else
            Nothing