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


 

