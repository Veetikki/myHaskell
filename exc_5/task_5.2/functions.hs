module Functions
(
    readPhone,
    makeCountryCode,
    makePhoneNo,
    toPhone,
    fixCc,
) where

import Data

{--Arrange your code of 5.1 into hierarchical modules 
so that there is a base module (directory) with two sub-modules (files in it), 
the first with all data definitions and the second with the functions.

I didn't comment others since only part was to split code
--}


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

makeCountryCode :: Integer -> CountryCode 
makeCountryCode cC  
    | cC < 0 = error "Negative value not allowed"
    | otherwise = CountryCode cC


makePhoneNo :: Integer -> PhoneNo
makePhoneNo pNo 
    | pNo < 0 = error "Negative value not allowed"
    | otherwise = PhoneNo pNo


toPhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
toPhone pT cC pNo = Phone pT cC pNo