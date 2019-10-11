main = loop []
  
loop :: [EventInfo] -> IO ()

loop ioEvents =
  do
    input <- getLine
    if input == "Quit"
    then putStrLn("bye")
    else doCommand input ioEvents

doCommand :: String -> [EventInfo] -> IO ()
doCommand input events
    | (length inputl == 6) = do 
      newEvents <- (doEvent inputl events)
      loop newEvents
    | (length inputl == 2 && (head inputl) == "Tell me about ") = doTell inputl events 
    | ((length inputl == 2) && (head inputl) == "What happens on ") = doHappensOn inputl events
    | ((length inputl == 2) && (head inputl) == "What happens at ") = doHappensAt inputl events
    | otherwise = do 
      printError
      loop events
    where inputl = (splitStr input '\'')