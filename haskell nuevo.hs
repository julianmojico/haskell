{- 

--instance Clase a where
funcion1 = 
funcion2 = 
....


(==) :: (Eq a) => a -> a -> Bool
Everything before the => symbol is called a class constraint. We can read the previous type declaration 
like this: the equality function takes any two values that are of the same type and returns a Bool. 
The type of those two values must be a member of the Eq class (this was the class constraint).


Listas por comprension:

ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
ghci> [a+b | (a,b) <- xs]  

he x:xs pattern is used a lot, especially with recursive functions. But patterns that have : in them only match against lists of length 1 or more.

**GUARDAS**   --SON COMO EL CASE?

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  


**BINDINGS**

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
	
**DATA TYPE**

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
	
-}



--practica1

doubleMe x = x + x

--import Text.Regex.Posix ((=~))

signo :: Int -> Int
signo 0 = 0
signo x = if x>0 then 1 else -1

negativo :: Int -> Bool
negativo x = signo x == -1

max :: Int -> Int -> Int
max x y = if x>y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = Main.max (Main.max x y) z	  --hay alguna manera mejor de escribirlo?

minim x y = if Main.max x y == x then y else x 



factorial x = if x==0 then 1 else factorial (x-1)*x

--o de otra manera...

factorial2 0 = 1
factorial2 n = n*factorial (n-1)

combinatorio x y = if x>y then factorial x `div` factorial y*factorial (x-y) else 0 --como se puede optimizar?

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x =  fibonacci (x-1) + fibonacci (x-2)


divisiblePor :: Integral a => a -> a -> Bool			
divisiblePor x y 
	| x `div` y == 0 = True
	| x `div` y == 1 = False

--div :: Integral a => a -> a -> a


--como extender los tipos que acepta una funcion? Haciendo funcion::(Tipo1,Tipo2)->a
-- ...pero sin redefinir la funcion??


--practica2

data Nat = Cero | Sig Nat deriving (Eq)

nat2Int::Nat->Int
nat2Int x
	|x==Cero = 0
	|otherwise = 1 + nat2Int x


data Poste = Src | Dst | Aux deriving (Show)
data Movimiento = Mov (Poste,Poste) deriving (Show)

hanoi :: Int->Poste->Poste->Poste->[Movimiento]
hanoi 0 s d a = [Mov(s,d)]
hanoi n s d a =  ( hanoi (n-1) s a d )++[Mov(s,d)]++( hanoi (n-1) a d s )

data Extension a = Undef | Solo a deriving (Show)

cabeza::[a]->Extension a
cabeza [] = Undef
cabeza (x:xs) = Solo x

data Factor = Negativo | Positivo deriving (Eq)
data TipoDeSangre =  TipoSangre Char Factor deriving (Eq)

puedeDonar::TipoDeSangre->TipoDeSangre->Bool
puedeDonar (TipoSangre x y) (TipoSangre z w) 
	| (x==z)  && (y==w) = True
	| (x == 'O') = True
	| otherwise = False



{- ejemplos para TP
data ListOrd a = ListOrd [a]

addListOrd::a->ListOrd a->ListOrd a
addListOrd x ListOrd (w:y:ys) 
	| x < w = ListOrd (x:w:y:ys)


let <bindings> in <expression>
-}
