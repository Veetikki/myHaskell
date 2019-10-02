-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List' a = Empty | Cons {listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord)

-- binary tree:
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton x = Node x EmptyTree EmptyTree

treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x==y = Node y left right -- no duplicate keys
  | x<y = Node y (treeInsert x left) right
  | x>y = Node y left (treeInsert x right)

treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x==y = True
  | x<y  = treeElem x left
  | x>y  = treeElem x right

type PhoneBook = Tree (String, [Phone])

--If no entry then returs empty list
findEntries :: String -> PhoneBook -> [Phone]
findEntries nm EmptyTree = []
findEntries nm (Node (y, z) left right)
    | nm == y = z
    | nm < y  = findEntries nm left
    | nm > y  = findEntries nm right

--Adds new entry
addEntry  :: String
          -> String
          -> String
          -> String
          -> PhoneBook 
          -> PhoneBook 
addEntry nm ptStr ccStr pnStr EmptyTree = singleton (nm, [(readPhone ptStr ccStr pnStr)])
addEntry nm ptStr ccStr pnStr (Node (y, z) left right)
    | nm < y = Node (y, z) (addEntry nm ptStr ccStr pnStr left) right
    | nm > y = Node (y, z) left (addEntry nm ptStr ccStr pnStr right)
    | otherwise = --We want to check phone numbers
        let newPhone = readPhone ptStr ccStr pnStr 
            findPhoneNos = map phoneNo $ findEntries nm (Node (y, z) left right)
            numberCheck = (toPhoneNo $ read pnStr) `elem` findPhoneNos
        in case numberCheck of
        True  -> (Node (y, z) left right) --We don't want to add duplicates
        False -> (Node (y, ( newPhone:z )) left right)
        
    

{--
addEntry nm ptStr ccStr pnStr oldBook =
  let newEntry = PhoneBookEntry nm $ readPhone ptStr ccStr pnStr --Note that haskell is lazy and would not try to evaluate this if the name and number combo is already in the phone book. This might or might not be what you want to do.
      findPhoneNos = map phoneNo $  map phone $ findEntries nm oldBook
      numberCheck = (toPhoneNo $ read pnStr) `elem` findPhoneNos
  in case numberCheck of
  True  -> oldBook            -- <-Here the newEntry would not be evaluated.
  False -> newEntry : oldBook -- <-Here the newEntry would be evaluated.


--}

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
    $ addEntry "PersonC" "WorkLandline"  "358"    "12312123" EmptyTree --This line is evaluated first

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

