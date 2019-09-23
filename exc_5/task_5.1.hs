{--
Take task 4.3 (You can use the example solution) and make the following changes to it:
-Use newtype instead of data wherever you can. (Easiest is probably to check from the dates.hs example file how to use newtype.)
-Change the definition of Phone so that country code and phone type are optional using Maybe.
-Change the Show instance of Phone so that it does not show country code or phone type if they are Nothing.
-Make the readPhone function accept empty strings for phone type and country code. If they are empty make them Nothing.
--}



--Taks 4.3
--Could be done differently with toPhone function
readPhone :: String -> String -> String -> Phone
readPhone [] [] pNo = Phone { phoneType = Nothing, countryCode = Nothing, phoneNo = makePhoneNo(read pNo :: Integer) }
readPhone [] cC pNo = Phone { phoneType = Nothing, countryCode = (Just (fixCc(cC))), phoneNo = makePhoneNo(read pNo :: Integer) }
readPhone pT [] pNo = Phone { phoneType = (Just (read pT)), countryCode = Nothing, phoneNo = makePhoneNo(read pNo :: Integer) }
readPhone pT cC pNo = Phone { phoneType = (Just (read pT)), countryCode = (Just (fixCc(cC))), phoneNo = makePhoneNo(read pNo :: Integer) }

fixCc :: String -> CountryCode
fixCc (s1:s2:ss)
    | s1 == '+' = let a = read (s2:ss) :: Integer in if a `elem`[1,44,52,91,86,358] then makeCountryCode a else error "Wrong type input in countryCode"
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

newtype CountryCode = CountryCode Integer deriving (Eq, Read)
instance Show CountryCode where 
    show (CountryCode cC) = "+" ++ show(cC)

makeCountryCode :: Integer -> CountryCode 
makeCountryCode cC  
    | cC < 0 = error "Negative value not allowed"
    | otherwise = CountryCode cC

newtype PhoneNo = PhoneNo Integer deriving (Eq, Read)
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
    phoneType :: Maybe PhoneType,
    countryCode :: Maybe CountryCode,
    phoneNo :: PhoneNo
    } deriving (Eq)

{--Derive an instance for Eq for it, but for Show make it "pretty-print" the infromation in this form:
<country code><space><phone number><space><phone type in parenthesis>
e.g. +358 123456789 (WorkLandline)--}
instance Show Phone where
    show (Phone Nothing Nothing pNo) = show (pNo)
    show (Phone Nothing (Just cC) pNo) = show (cC) ++ " " ++ show (pNo)
    show (Phone (Just pT) Nothing pNo) = show (pNo) ++ " (" ++ show(pT) ++ ")"
    show (Phone (Just pT) (Just cC) pNo) = show (cC) ++ " " ++ show (pNo) ++ " (" ++ show(pT) ++ ")"
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


toPhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
toPhone pT cC pNo = Phone pT cC pNo