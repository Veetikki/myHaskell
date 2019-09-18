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