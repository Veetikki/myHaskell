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