--I excepted that user knows right values for PhoneType


--Let's make different types 
--Using the data keyword, define PhoneType type that has constructors for values WorkLandline, PrivateMobile, WorkMobile, and Other.
--Derive instance for Show, Eq and Read for it.

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

--Using the type keyword, define two type synonyms for Integer: CountryCode and PhoneNo.

type CountryCode = Integer
type PhoneNo = Integer

{--
Then, using the record syntax, define Phone type for phone numbers that has only one value constructor with fields for
phoneType :: PhoneType,
countryCode :: CountryCode, (just a type synonym for Integer)
and phoneNo :: PhoneNo. (just a type synonym for Integer)
Derive instances for Show, Eq and Read for it.
--}

data Phone = Phone {
    phoneType :: PhoneType,
    countryCode :: CountryCode,
    phoneNo :: PhoneNo
    } deriving (Show, Eq, Read)

{--
Make a function of type
:: PhoneType
-> CountryCode (just a type synonym for Integer)
-> PhoneNo (just a type synonym for Integer)
-> Phone
that throws an error if CountryCode or PhoneNo is a negative integer and otherwise creates a value of type Phone with the given values.
--}

myFunc :: PhoneType -> CountryCode -> PhoneNo -> Phone
myFunc pT cC pN 
    | pN < 0 || cC < 0 = error "Wrong input for PhoneNo. or CountryCode"
    | otherwise = Phone {phoneType = pT, countryCode = cC, phoneNo = pN}