data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving(Show, Read, Eq)

--      +-- Type constructor
--      |
--      |               +--Value/data constructor
--      |               |
--      |               |  Type and value/data constructor could have the same name; They are in different namespaces.
--      v               v  It is conventional to name them the same, but now for clarity we name them differently.
data CountryCode  = MakeCountryCode Integer deriving(Eq,Read)
data PhoneNo      = MakePhoneNo Integer deriving(Eq,Read)
data Phone        = MakePhone {phoneType   :: Maybe PhoneType
                              ,countryCode :: Maybe CountryCode
                              ,phoneNo     :: PhoneNo    
                              } deriving(Eq, Read)

toCountryCode :: Integer -> Maybe CountryCode
toCountryCode x | x < 0 = error "Invalid country code."
                | otherwise = MakeCountryCode x
                  
toPhoneNo :: Integer -> PhoneNo
toPhoneNo x | x < 0 = error "Invalid phone number."
            | otherwise = MakePhoneNo x
              
{- This is actually the same as the data/value constructor for Phone. You can try :t MakePhone in ghci to see the type of the data/value constructor.
-}
toPhone :: Maybe PhoneType
          -> Maybe CountryCode
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