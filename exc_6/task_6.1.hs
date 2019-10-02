--6.1 Change readPhone function in 5.1 to use readMaybe (given at https://www.sis.uta.fi/~csjtn/fp/) 
--to read the values from the strings (ie do not check for empty strings, you will get Nothing from the readMaybe in that case anyway).

-- reads: if successful, returns a single element list
-- and it also returns the part of the string it did not consume
--
-- It could be used for reading more things from the list
-- Or, as in the following readMaybe, we can test if the 
-- string contained exactly the right input:

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing 


-- remember that the function words can be used to
-- split an input string into "words" so you can
-- extract the parts of strings and apply readMaybe
-- For more complicated parsing, there are other solutions.

--Taks 5.1
--Could be done differently with toPhone function
--Dunno why you can't just read CountryCode
readPhone :: String -> String -> String -> Phone
readPhone pT cC pNo = Phone { phoneType = (readMaybe pT), countryCode = makeMaybeCountryCode((readMaybe (fixCc(cC))) :: Maybe Integer), phoneNo = makePhoneNo(read pNo) }

makeMaybeCountryCode :: Maybe Integer -> Maybe CountryCode 
makeMaybeCountryCode Nothing = Nothing
makeMaybeCountryCode (Just cC) = let a = makeCountryCode cC in (Just a)

fixCc :: String -> String
fixCc "" = ""
fixCc (s1:s2:ss)
    | s1 == '+' = if (s2:ss) `elem`["1","44","52","91","86","358"] then (s2:ss) else error "Wrong type input in countryCode"
    | s1 == '0' && s2 == '0' = if (ss) `elem`["1","44","52","91","86","358"] then (ss) else error "Wrong type input in countryCode"
    | otherwise = if (s1:s2:ss) `elem`["1","44","52","91","86","358"] then (s1:s2:ss) else error "Wrong type input in countryCode"


data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

{--Now, insted of using type synonyms, define data types CountryCode and PhoneNo so that both of them have a value constructor that takes an integer.
Derive instances for Eq for them and make Show instances for them so that:
CountryCode: print '+' in front of the number.
PhoneNo: print only the number.
Make a function for both of them that takes and Integer and throws an error if the integer is negative otherwise it creates the value.--}

newtype CountryCode = CountryCode Integer deriving(Eq, Read)
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


toPhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
toPhone pT cC pNo = Phone pT cC pNo