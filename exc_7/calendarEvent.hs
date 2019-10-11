{--
The exercise is about calendar events (simplified), having a date, a place, and a name.

The user inputs you need to manage are:
Event <name> happens at <place> on <date>   --> Ok or Bad date
Tell me about <eventname>                   --> Event <name> happens at <place> on <date> or I do not know of such event
What happens on <date>                      --> Event <name> happens on <date> or Nothing that I know of
What happens at <place>                     --> Event <name> happens at <place> or Nothing that I know of
What happens at <place> on <date>           --> Event <name> happens at <place> on <date> or Nothing that I know of
Quit

The values are given inside of quotation marks and the format of the date is YYYY-MM-DD:
Event 'Event X' happens at 'Place Y' on '2019-10-08'
Tell me about 'Event X'
What happens on '2019-10-08'
What happens at 'Place Y'
Quit

No need to care about whitespaces

If the input is not one of those (including format problems), the response is:
I do not understand that. I understand the following:
*Event <name> happens at <place> on <date>
*Tell me about <eventname>
*What happens on <date>
*What happens at <place>
*Quit


--}

-- THE FOLLOWING WILL STILL CHANGE SLIGHTLY, AND WE WILL PROVIDE A TEMPLATE FOR doCOmmand
-- The idea anyway is that doCommand will eventually call loop with the updated event list
import System.IO
import Data.List (sortBy) --For sorting events

data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

instance Show EventInfo where
  show(EventInfo n p d) =  "Event " ++ n ++ " happens at " ++ p ++" on " ++ show(d)

main = loop $ return []
  
loop :: IO [EventInfo] -> IO ()

loop ioEvents =
  do
    input <- getLine
    if input == "Quit"
    then putStrLn("bye")
    else doCommand input ioEvents

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents 
    | (length inputl == 6) = do --we check on other function that all others are correct 
      events <- ioEvents
      newEvents <- (doEvent inputl events) 
      loop $ return newEvents
    | (length inputl == 2 && (head inputl) == "Tell me about ") = do --Tell me about command
      events <- ioEvents
      doTell (last inputl) events
      loop $ return events 
    | ((length inputl == 2) && (head inputl) == "What happens on ") = do --What happens on
      events <- ioEvents
      doHappensOn (last inputl) events
      loop $ return events
    | ((length inputl == 2) && (head inputl) == "What happens at ") = do --What happens at
      events <- ioEvents
      doHappensAt (last inputl) events
      loop $ return events  
    | otherwise = do --Otherwise command is not valid
      events <- ioEvents
      printError
      loop $ return events
    where inputl = (splitStr input '\'')


--This splits given String by given Char
splitStr :: String -> Char -> [String]
splitStr [] _ = []
splitStr s c = x : splitStr (drop 1 y) c where (x,y) = span (/= c) s --cut the string when we find given char

sortDate (EventInfo n1 p1 d1) (EventInfo n2 p2 d2) = compare d1 d2 --helper function for sortby to sort with date

sortName (n1,d1) (n2,d2) = compare n1 n2 --helper for sorting by name

--Function to Tell me about command
doTell :: String -> [EventInfo] -> IO ()
doTell [] events = do putStrLn("I do not know of such event")
doTell _ [] = do putStrLn("I do not know of such event")
doTell event events = let l = [(EventInfo n p d) | (EventInfo n p d) <- events, event == n] in if length l == 1 then putStrLn(show $ head l) else putStrLn("I do not know of such event") --There only must be one eventinfo for every eventname

doHappensOn :: String -> [EventInfo] -> IO ()
doHappensOn _ [] = do putStrLn("Nothing that I know of")
doHappensOn [] _ = do putStrLn("Nothing that I know of")
doHappensOn time events = let l = [(n, d) | EventInfo n p d <- events, time == show(d)] in if length l /= 0 then printEvents $ (sortBy sortName l) else putStrLn("Nothing that I know of") --we wanted to sort these ones by name
      where printEvents :: [(String, Date)] -> IO () --prints all events
            printEvents [] = putStr("")
            printEvents (e:es) = do
              putStrLn("Event " ++ fst e ++ " happens on " ++ show(snd e))
              printEvents es

doHappensAt :: String -> [EventInfo] -> IO ()
doHappensAt _ [] = do putStrLn("Nothing that I know of")
doHappensAt [] _ = do putStrLn("Nothing that I know of")
doHappensAt place events = let l = [(n, p) | EventInfo n p d <- (sortBy sortDate events), p == place]in if length l /= 0 then printEvents l else putStrLn("Nothing that I know of") --we sort theses ones by date
      where printEvents :: [(String, String)] -> IO () --prints events
            printEvents [] = putStr("")
            printEvents (e:es) = do
              putStrLn("Event " ++ fst e ++ " happens at " ++ snd e)
              printEvents es

--Adds events to list
doEvent :: [String] -> [EventInfo] -> IO [EventInfo]
doEvent [] events = do return events
doEvent (s1:s2:s3:s4:s5:s6:_) events
    | s1 == "Event " && s3 == " happens at " && s5 == " on " = let d = parseDate s6 --check that command is valid and we want to parse given date and check that it is valid
      in if(d /= Nothing) 
          then do
            putStrLn("Done")
            return ((EventInfo {name = s2, place = s4, date = (fromJust d)}) : (removeEvent s2 events)) --we want to make sure that we remove old data
        else do
          putStrLn("Bad day")
          return events
    | otherwise = do
      printError
      return events
    where fromJust :: Maybe a -> a --removes just
          fromJust Nothing  = error "Maybe.fromJust: Nothing"
          fromJust (Just x) = x

--removes with given name
removeEvent :: String -> [EventInfo] -> [EventInfo]
removeEvent s events = [EventInfo n p d | (EventInfo n p d) <- events, n /= s]

--Prints if not valid command
printError :: IO ()
printError = do 
  putStrLn("I do not understand that. I understand the following:")
  putStrLn("*Event <name> happens at <place> on <date>")
  putStrLn("*Tell me about <eventname>")
  putStrLn("*What happens on <date>")
  putStrLn("*What happens at <place>")
  putStrLn("*Quit")

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing 
--Parses date    
parseDate :: String -> Maybe Date
parseDate s = let ss = words (map (\x -> if (x == '-') then ' ' else x) s) in if (length ss /= 3) then Nothing else readDate (ss!!0) (ss!!1) (ss!!2) --replace '-' with ' ' then take words

--reads date with string
readDate :: String -> String -> String -> Maybe Date
readDate strY strM strD
    | correctDate y m d = Just (makeDate y m d)
    | otherwise = Nothing
        where
            y = read strY :: Integer
            m = read strM :: Integer
            d = read strD :: Integer

data Month = MakeMonth Integer deriving (Eq, Ord)

--instance to make rigth show
instance Show Month where
  show(MakeMonth m) = show(m)
-- If all values are ok, it is enough to call "MakeMonth x"
-- Now the input is an Integer. What if it was a String?
-- - then MakeMonth (read x :: Integer) would do it
toMonth               :: Integer -> Month
toMonth x
  | x < 1     = error "Minimum month number is 1" 
  | x > 12     = error "Maximum month number is 12" 
  | otherwise = MakeMonth x

fromMonth             :: Month -> Integer
fromMonth (MakeMonth i) = i  -- Pattern match i out 

-- This is done similarly as Month
data Day = MakeDay Integer deriving (Eq, Ord)

instance Show Day where
  show(MakeDay d) = show(d)

toDay               :: Integer -> Day
toDay x
  | x < 1     = error "Minimum day number is 1" 
  | x > 31     = error "Maximum day number is 31" 
  | otherwise = MakeDay x

fromDay             :: Day -> Integer
fromDay (MakeDay i) = i 

newtype Year = MakeYear Integer deriving (Eq, Ord)

instance Show Year where
  show(MakeYear y) = show y

toYear :: Integer -> Year
toYear x
 | x == 0 = error "No year 0"
 | otherwise = MakeYear x

fromYear :: Year -> Integer
fromYear (MakeYear x) = x

-- A record type. Notice that we get functions year, month, and day
-- that are used in the functions below. The record field names
-- year, month and day are also used in assignign values - see below.

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)

--this adds zeros if date or moth is less than 10
instance Show Date where
  show(Date y m d) = show(y) ++ "-" ++ m' ++ "-" ++ d'
    where m' = if (m < 10 ) then "0" ++ show(m) else show(m)
          d' = if (d < 10 ) then "0" ++ show(d) else show(d)
-- Examples of applications of the types:

-- A function to check if a year is a leap year

leapYear (MakeYear y)
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

-- 3: Write a function to check if a given date (y,m,d)
--    is correct

correctDate :: Integer -> Integer -> Integer -> Bool
correctDate 0 _ _  = False
correctDate y m d
 | (elem m [1,3,5,7,8,10,12]) && (elem d [1..31]) = True
 | (elem m [4,6,9,11]) && (elem d [1..30]) = True
 | (m==2) && (elem d [1..28]) = True
 | (leapYear (toYear y)) && (m==2) && (d==29) = True
 | otherwise = False


makeDate :: Integer -> Integer -> Integer -> Date
makeDate y m d
 | correctDate y m d = Date { year = toYear y, month = toMonth m, day = toDay d }
 | otherwise = error "not correct combination of integers for year, month and day"


-- 4: Write a function that, given a date,
--    calculates the next date

nextDate :: Date -> Date
nextDate date
  | correctDate y m (d+1) =  Date { year = year date, month = month date, day = toDay (d+1) }
  | correctDate y (m+1) 1 = Date { year = year date, month = toMonth (m+1), day = toDay 1  }
  | y == (-1) = Date { year = toYear 1, month = toMonth 1, day = toDay 1 }
  | otherwise = Date { year = toYear (y+1), month = toMonth 1, day = toDay 1 }
  where y = fromYear $ year date
        m = fromMonth $ month date
        d = fromDay $ day date


-- 5: distance of two dates

dateDistance :: Date -> Date -> Integer
dateDistance date1 date2
  | not (correctDate y1 m1 d1) = error "only calculate distance for correct dates"
  | not (correctDate y2 m2 d2) = error "only calculate distance for correct dates"
  | date1 == date2 = 0
  | date1 < date2 = addDateUntil' date1 date2 0 -- tail recursion
  | otherwise = addDateUntil date2 date1 -- not tail recursion
  where y1 = fromYear $ year date1
        m1 = fromMonth $ month date1
        d1 = fromDay $ day date1
        y2 = fromYear $ year date2
        m2 = fromMonth $ month date2
        d2 = fromDay $ day date2


-- This would not use tail recursion
addDateUntil date1 date2 
  | date1 == date2 = 0
  | otherwise = 1 + addDateUntil (nextDate date1) date2

-- this one uses tail recursion
addDateUntil' date1 date2 n
  | date1 == date2 = n
  | otherwise = addDateUntil' (nextDate date1) date2 (n+1)
 


-- This is getting out of the course contents, but out of interest:
-- The following makes Month to be a number in the Num typeclass
-- If no calculations are needed, this is not needed.
-- I do not define abs or signum. It seems to go through.
-- I only allow positive values so they are not relevant.

instance Num Month where
    fromInteger         = toMonth
    x + y               = let r = fromMonth x + fromMonth y in
                            if r < 1 || r > 12 then error "Unnatural addition for month"
                                     else toMonth r
    x - y               = let r = fromMonth x - fromMonth y in
                            if r < 1 || r > 12 then error "Unnatural subtraction for month"
                                     else toMonth r
    x * y               = let r = fromMonth x * fromMonth y in
                            if r < 1 || r > 12 then error "Unnatural multiplication for month"
                                     else toMonth r

instance Num Day where
    fromInteger         = toDay
    x + y               = toDay $ fromDay x + fromDay y
    x - y               = toDay $ fromDay x - fromDay y
    x * y               = toDay $ fromDay x * fromDay y