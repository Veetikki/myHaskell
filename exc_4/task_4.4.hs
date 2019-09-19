{--4. Use the types and functions you have created in the previous tasks in this task.

Make a record that contains simple phone book entry information:
Name
Phone

Suppose we have a list of phone book entries. Make functions to:

-Find a list of entries by a name. (All entries where the name is the given one)

-Add a new entry, given a string for the name, the three strings for the phone like in Task 4.3 and the list of phone book entries to add the new entry to. 
If there already exists an entry with the given name and the given number(phoneNo field in Phone), then make no change.--}

data Record = Record {
    name :: String,
    phone :: Phone
    } deriving(Eq)

type PhoneBook = [Record]    

instance Show Record where
    show (Record n p) = show (n) ++ " " ++ show (p)

--Example list
simpleList = [ Record {name = "Matti", phone = readPhone "Other" "+358" "123456789"}, Record {name = "Teppo", phone = readPhone "WorkLandline" "+1" "234567890"}, Record {name = "Sirpa", phone = readPhone "PrivateMobile" "+44" "112233445"}, Record {name = "Daniel", phone = readPhone "PrivateMobile" "91" "223344556"}, Record {name = "Daniel", phone = readPhone "WorkMobile" "91" "334455667"}]
    
--Check if name in list
listWithName :: PhoneBook -> String -> PhoneBook
listWithName r s = [ Record n p | (Record n p) <- r, n == s]

{--
-Add a new entry, given a string for the name, the three strings for the phone like in Task 4.3 and the list of phone book entries to add the new entry to. 
If there already exists an entry with the given name and the given number(phoneNo field in Phone), then make no change.--}
addRecord :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addRecord n pT cC pNo [] = [Record {name = n, phone = readPhone pT cC pNo}]
addRecord n pT cC pNo p = let r = Record {name = n, phone = readPhone pT cC pNo} in if r `elem` p then error "already in list" else r : p

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