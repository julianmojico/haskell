--practica1

doubleMe x = x + x
signo :: Integer -> Int
signo 0 = 0
signo x = if x>0 then 1 else -1

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

-- factorial x = if x==0 then 1 else x*factorial x-1 --entro en loop infinito y me tiro out of memory


factorial x = if x==0 then 1 else factorial (x-1)*x

--o de otra manera...

factorial2 0 = 1
factorial2 n = n*factorial (n-1)

combinatorio x y = if x>y then factorial x `div` factorial y*factorial (x-y) else 0 --como se puede optimizar?

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x =  fibonacci (x-1) + fibonacci (x-2)

--divisiblePor :: Int -> Int -> Bool

div2 :: (Integral a, Fractional a) => a -> a -> a
div2 x y = x `div` y

divisiblePor :: (Ord a) => a -> a -> Bool			--como extender los tipos que acepta una funcion?
divisiblePor x y = (x `div` y) < (x / y)

--practica2

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
data TipoDeSangre =  TipoSangre String Factor deriving (Eq)

--instance Eq -> TipoDeSangre where
--(TipoSangre String a b)==(TipoSangre String c d) = (a==c) && (b==d) 

puedeDonar::TipoDeSangre->TipoDeSangre->Bool
puedeDonar  (TipoSangre (String x) _) (TipoSangre (String y) _) = (x==y)
puedeDonar (TipoSangre "O" _) (TipoSangre z m) = True




 

