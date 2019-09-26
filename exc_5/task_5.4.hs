{--Below you can find a definition for 3-valued Boolean that has values True3, False3, and Unk3 and a definition of a MaybeNull type constructor.

Make a typeclass Eq3 which has a function (===) that is the 3-valued equivalence, that is, comparing two values gives a result of type Bool3.

Then, make Bool3 a member of the typeclass Eq3.

Then, make MaybeNull a member of the typeclass Eq3. Use the database semantics, where we do not know if Null == Null.
--}

class Eq3 a where
    (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
    True3   === True3 = True3
    False3  === False3 = True3
    Unk3    === Unk3 = Unk3
    True3   === Unk3 = Unk3
    False3  === Unk3 = Unk3
    Unk3    === True3 = Unk3
    Unk3    === False3 = Unk3
    _       === _ = False3 

instance (Eq3 a) => Eq3 (MaybeNull a) where
    JustVal x   === JustVal y   = x === y
    Null        === Null        = Unk3

-- MaybeNull:

data MaybeNull a = JustVal a | Null

-- Bool3:

data Bool3 = False3 | Unk3 | True3 deriving (Eq,Show) 

(&&&) :: Bool3 -> Bool3 -> Bool3
(&&&) x y
 | x == True3 && y == True3 = True3
 | x == False3 || y == False3 = False3
 |otherwise = Unk3

(|||) :: Bool3 -> Bool3 -> Bool3
(|||) x y
 | x == True3 || y == True3 = True3
 | x == False3 && y == False3 = False3
 |otherwise = Unk3

not3 :: Bool3 -> Bool3
not3 x
 | x == True3 = False3
 | x == False3 = True3
 |otherwise = Unk3