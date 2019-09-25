module Data
(
    PhoneType,
    CountryCode(CountryCode),
    PhoneNo(..),
    Phone(..),
) where

{--Arrange your code of 5.1 into hierarchical modules 
so that there is a base module (directory) with two sub-modules (files in it), 
the first with all data definitions and the second with the functions.

I didn't comment others since only part was to split code
--}

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

newtype CountryCode = CountryCode Integer deriving (Eq, Read)
instance Show CountryCode where 
    show (CountryCode cC) = "+" ++ show(cC)

newtype PhoneNo = PhoneNo Integer deriving (Eq, Read)
instance Show PhoneNo where
    show (PhoneNo pNo) = show(pNo)

data Phone = Phone {
    phoneType :: Maybe PhoneType,
    countryCode :: Maybe CountryCode,
    phoneNo :: PhoneNo
    } deriving (Eq)

instance Show Phone where
    show (Phone Nothing Nothing pNo) = show (pNo)
    show (Phone Nothing (Just cC) pNo) = show (cC) ++ " " ++ show (pNo)
    show (Phone (Just pT) Nothing pNo) = show (pNo) ++ " (" ++ show(pT) ++ ")"
    show (Phone (Just pT) (Just cC) pNo) = show (cC) ++ " " ++ show (pNo) ++ " (" ++ show(pT) ++ ")"
