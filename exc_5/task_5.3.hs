import qualified Data.Map as Map
import Data.Map (Map)

{--4. Use the types and functions you have created in the previous tasks in this task.

Make a record that contains simple phone book entry information:
Name
Phone

Suppose we have a list of phone book entries. Make functions to:

-Find a list of entries by a name. (All entries where the name is the given one)

-Add a new entry, given a string for the name, the three strings for the phone like in Task 4.3 and the list of phone book entries to add the new entry to. 
If there already exists an entry with the given name and the given number(phoneNo field in Phone), then make no change.--}
type PhoneBook = Map String [Phone]

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
  $ addEntry "PersonC" "WorkLandline"  "358"    "12312123" Map.empty --This line is evaluated first

--Add entry to map if with same name then it adds value to the list
addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry n pT cC pNo pB
    | listWithName n pB == [] = Map.insert n [(readPhone pT cC pNo)] pB
    | otherwise = let a = listWithName n pB in if checkPhoneNumber a (makePhoneNo (read pNo)) == False then  Map.insertWith (++) n [(readPhone pT cC pNo)] pB else pB
    where
        checkPhoneNumber :: [Phone] -> PhoneNo -> Bool
        checkPhoneNumber [] _ = False
        checkPhoneNumber ((Phone pT1 cC1 pNo1) : xs) pNo2
            | pNo1 == pNo2 = True
            | otherwise = checkPhoneNumber xs pNo2

--This list phonenumbers with given name
listWithName :: String -> PhoneBook -> [Phone]
listWithName n pB
    | Map.lookup n pB == Nothing = []
    | otherwise = let Just a = Map.lookup n pB in a

--Taks 4.3
readPhone :: String -> String -> String -> Phone
readPhone pT cC pNo = Phone { phoneType = read pT :: PhoneType, countryCode = fixCc(cC), phoneNo = makePhoneNo(read pNo :: Integer) }

fixCc :: String -> CountryCode
fixCc (s1:s2:ss)
    | s1 == '+' = let a = read (s2:ss) :: Integer in if a `elem`[1,44,52,91,86,358] then  makeCountryCode a else error "Wrong type input in countryCode"
    | s1 == '0' && s2 == '0' = let a = read ss :: Integer in if a `elem` [1,44,52,91,86,358] then makeCountryCode a else error "Wrong type input in countryCode"
    | otherwise = let a = (read (s1:s2:ss) :: Integer) in if a `elem` [1,44,52,91,86,358] then makeCountryCode a else error "Wrong type input in countrycode"


--I excepted that user knows right values for PhoneType

{--
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
So I excepted that you need to create your own Countrycodes e.g. let a = makeCountryCode 358 and let b = makePhoneNo 123456789
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--}

--Let's make different types 
--Using the data keyword, define PhoneType type that has constructors for values WorkLandline, PrivateMobile, WorkMobile, and Other.
--Derive instance for Show, Eq and Read for it.

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

{--Now, insted of using type synonyms, define data types CountryCode and PhoneNo so that both of them have a value constructor that takes an integer.
Derive instances for Eq for them and make Show instances for them so that:
CountryCode: print '+' in front of the number.
PhoneNo: print only the number.
Make a function for both of them that takes and Integer and throws an error if the integer is negative otherwise it creates the value.--}

data CountryCode = CountryCode Integer deriving (Eq, Read)
instance Show CountryCode where 
    show (CountryCode cC) = "+" ++ show(cC)

makeCountryCode :: Integer -> CountryCode 
makeCountryCode cC  
    | cC < 0 = error "Negative value not allowed"
    | otherwise = CountryCode cC

data PhoneNo = PhoneNo Integer deriving (Eq, Read)
instance Show PhoneNo where
    show (PhoneNo pNo) = show(pNo)

makePhoneNo :: Integer -> PhoneNo
makePhoneNo pNo 
    | pNo < 0 = error "Negative value not allowed"
    | otherwise = PhoneNo pNo
{--
Then again, using the record syntax, define Phone type for phone numbers that has only one value constructor with fields
phoneType :: PhoneType,
countryCode :: CountryCode, (This time a type of its own)
and phoneNo :: PhoneNo. (This time a type of its own)
--}

data Phone = Phone {
    phoneType :: PhoneType,
    countryCode :: CountryCode,
    phoneNo :: PhoneNo
    } deriving (Eq)

{--Derive an instance for Eq for it, but for Show make it "pretty-print" the infromation in this form:
<country code><space><phone number><space><phone type in parenthesis>
e.g. +358 123456789 (WorkLandline)--}
instance Show Phone where
    show (Phone pT cC pNo) = show (cC) ++ " " ++ show (pNo) ++ " (" ++ show(pT) ++ ")"
{--
Make a function of type
:: PhoneType
-> CountryCode (This time a type of its own)
-> PhoneNo (This time a type of its own)
-> Phone
but this time do not worry about the values of CountryCode and PhoneNo, since if they are created with the functions you made before they are already correct.
--}

{--
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
So, I excepted that you need to create your own Countrycodes e.g. let a = makeCountryCode 358 and let b = makePhoneNo 123456789
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--}

myFunc :: PhoneType -> CountryCode -> PhoneNo -> Phone
myFunc pT cC pNo = Phone {phoneType = pT, countryCode = cC, phoneNo = pNo}