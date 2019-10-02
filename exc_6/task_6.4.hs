--6.4. Make an interactive phone book application that only allows for insertions and searching the values
import Control.Monad
import System.IO --This is to update putStr

main = do
    putStr("phonebook> ")
    hFlush stdout
    line <- getLine
    askForCommand line examplePhoneBook
    putStrLn("bye!")

askForCommand :: String -> PhoneBook -> IO ()
askForCommand line pB = do
    when (line /= "quit") $ do
        let (newpB, o) = (getOutputAndPhoneBook (splitStr line ' ') pB) in do
            putStrLn(o)
            putStr("phonebook> ")
            hFlush stdout
            newLine <- getLine
            askForCommand newLine newpB
                where
                    getOutputAndPhoneBook :: [String] -> PhoneBook -> (PhoneBook, String) --This function checks input and gives output
                    getOutputAndPhoneBook sTr pB
                        | (sTr!!0) == "add" && (length sTr == 5) = let newpB = (addEntry (sTr!!1) (sTr!!2) (sTr!!3) (sTr!!4) pB) in (newpB, "Done") --We will add and out will be done when completed
                        | (sTr!!0) == "find" && (length sTr == 2) = (pB, show(findEntries (sTr!!1) pB)) --output is the list of phone with given name
                        | otherwise = (pB, "Wrong Command") --athor commands are wrong
                
                    splitStr :: String -> Char -> [String]
                    splitStr [] _ = []
                    splitStr s c = x : splitStr (drop 1 y) c where (x,y) = span (/= c) s

--Task 4.4 as phonebook
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving(Show, Read, Eq)

--      +-- Type constructor
--      |
--      |               +--Value/data constructor
--      |               |
--      |               |  Type and value/data constructor could have the same name; They are in different namespaces.
--      v               v  It is conventional to name them the same, but now for clarity we name them differently.
data CountryCode  = MakeCountryCode Integer deriving(Eq,Read)
data PhoneNo      = MakePhoneNo Integer deriving(Eq,Read)
data Phone        = MakePhone {phoneType   :: PhoneType
                                ,countryCode :: CountryCode
                                ,phoneNo     :: PhoneNo    
                                } deriving(Eq, Read)

toCountryCode :: Integer -> CountryCode
toCountryCode x | x < 0 = error "Invalid country code."
                | otherwise = MakeCountryCode x
                    
toPhoneNo :: Integer -> PhoneNo
toPhoneNo x | x < 0 = error "Invalid phone number."
            | otherwise = MakePhoneNo x
                
{- This is actually the same as the data/value constructor for Phone. You can try :t MakePhone in ghci to see the type of the data/value constructor.
-}
toPhone :: PhoneType
            -> CountryCode
            -> PhoneNo
            -> Phone
toPhone pt cc pn = MakePhone pt cc pn

-- Type constructor |
--                  |
--                  v
instance Show CountryCode where
    show (MakeCountryCode x) = '+' : show x
--           ^
--           |
--           +-- Pattern matching the value/data constructor.

-- Type constructor |
--                  |
--                  v
instance Show PhoneNo where
    show (MakePhoneNo x) = show x
--           ^
--           |
--           +-- Pattern matching the value/data constructor.

instance Show Phone where
    show phone = show cc ++ " "++ show pn ++ " (" ++ show pt ++ ")" 
        where
            cc = countryCode phone
            pn = phoneNo phone
            pt = phoneType phone    
--         ^
--         |
--         +-- Functions that using the record syntax generated for us.

availableCountryCodes = ["358", "359", "41", "84"]

readPhone :: String
          -> String 
          -> String
          -> Phone
readPhone ptStr ccStr pnStr = 
  let pt = read ptStr -- The derived Read instance makes the read function read a value out of string which is formatted the same as using show with derived instance for Show. (There could also be parentheses around it e.g. "(((WorkLandline)))" would read the same value as "WorkLandline")
      ccStr' = removePrefix ccStr --Note that the implementation of this function is right below inside the where clause.
        where removePrefix ('+':xs) = xs
              removePrefix ('0':'0':xs) = xs
              removePrefix xs = xs
      cc = toCountryCode $ read ccStr'
      pn = toPhoneNo $ read pnStr
  in case ccStr' `elem` availableCountryCodes of
    True  -> toPhone pt cc pn
    False -> error "Unknown country code!"

examplePhoneBook =
    addEntry "PersonA" "WorkLandline"  "00358"  "123456789" --This line is evaluated last, but addEntry adds to the head of the list so it is there as the first element (if it is added)
    $ addEntry "PersonA" "PrivateMobile" "358"    "123456789"
    $ addEntry "PersonB" "Other"         "+358"   "123456789"
    $ addEntry "PersonB" "PrivateMobile" "358"    "123456789"
    $ addEntry "PersonA" "WorkLandline"  "00358"  "123456789"
    $ addEntry "PersonA" "WorkMobile"    "358"    "123456789"
    $ addEntry "PersonD" "WorkLandline"  "+358"   "123456789"
    $ addEntry "PersonA" "WorkLandline"  "358"    "123456789"
    $ addEntry "PersonA" "WorkMobile"    "00358"  "123456789"
    $ addEntry "PersonA" "WorkMobile"    "358"    "987654321"
    $ addEntry "PersonB" "WorkLandline"  "358"    "2323"        --And so on..
    $ addEntry "PersonB" "Other"         "+358"   "144"         --Now this call gets the phone book that is constructed below.
    $ addEntry "PersonC" "WorkLandline"  "358"    "12312123" [] --This line is evaluated first



data PhoneBookEntry = PhoneBookEntry { name  :: String
                                        , phone :: Phone
                                        } deriving(Eq, Show)

type PhoneBook = [PhoneBookEntry]

findEntries :: String -> PhoneBook -> PhoneBook
findEntries nm pb = filter (\x -> name x == nm) pb

addEntry  :: String
            -> String
            -> String
            -> String
            -> PhoneBook
            -> PhoneBook
addEntry nm ptStr ccStr pnStr oldBook =
    let newEntry = PhoneBookEntry nm $ readPhone ptStr ccStr pnStr --Note that haskell is lazy and would not try to evaluate this if the name and number combo is already in the phone book. This might or might not be what you want to do.
        findPhoneNos = map phoneNo $  map phone $ findEntries nm oldBook
        numberCheck = (toPhoneNo $ read pnStr) `elem` findPhoneNos
    in case numberCheck of
    True  -> oldBook            -- <-Here the newEntry would not be evaluated.
    False -> newEntry : oldBook -- <-Here the newEntry would be evaluated.