import Data.List 
import Data.Char
-- Exercício 1 

data Frac = F Integer Integer deriving Show

normaliza :: Frac -> Frac  -- calcula fracao irredutivel e com denominador positivo 
normaliza (F x y) = F (div x m) (div y m)             -- acrescentar deriving show
    where m = mdc x y 
-- normaliza (F (30) (10) )
-- = F 3 1

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc y (mod x y)

--mdc :: Integral a => a -> a -> a
--mdc a b | mod a b == 0 = b
----        | mod b a == 0 = a
--        | a > b = mdc b (mod a b)
--        | a < b = mdc a (mod b a)


instance Eq Frac where
   (F x y) == (F x' y') = compara (normaliza (F x y)) (normaliza (F x' y'))
    where 
        compara (F x y) (F x' y') = (x == x' && y == y')
--  (F x y) == (F x' y') = x * y' == x' * y   
-- (F 20 10) == (F 2 1)
-- = True

instance Ord Frac where
    compare (F x y) (F x' y') 
      | a < b = LT
      | a == b = EQ
      | a > b = GT
      where 
          a = (fromIntegral x / fromIntegral y)
          b = (fromIntegral x' / fromIntegral y')
-- compare (F 20 10) (F 2 1)
-- = EQ
-- compare (F 20 10) (F 2 0)
-- = LT
-- = compare (F 20 10) (F 0  3)
-- = GT


{--                               esta como comentario porque em cima ja definimos deriving show
instance Show Frac where
    show (F x y) = (show x ++ "/" ++ show y)
-- show (F 4 5)
-- = "4/5"
--}

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--class (Eq a, Show a) => Num a where
-- (+), (*), (-) :: a -> a -> a
--  negate, abs, signum :: a -> a
--  fromInteger :: Integer -> a

instance Num Frac where
    (+) = somaFrac
    (-) = subFrac
    (*) = multFrac 
    negate (F x y) = (F (-x) y)
    abs (F x y) = (F (abs x) (abs y))
    signum (F x y) = (F ((signum x) * (signum y)) 1 ) -- pq se quer o sinal positivo ou negativo so no numerador (sim é estupido mas yaa)
    fromInteger x = (F x 1)

somaFrac :: Frac -> Frac -> Frac 
somaFrac (F x y) (F x' y') = normaliza (F ((x*y') + (x'*y)) (y*y'))

subFrac :: Frac -> Frac -> Frac
subFrac (F x y) (F x' y') = normaliza (F ((x*y') - (x'*y)) (y*y'))

multFrac :: Frac -> Frac -> Frac
multFrac (F x y) (F x' y') = normaliza (F (x*x') (y*y'))

-- (-) (F 20 10) (F 3 5)
-- = F 7 5
--  signum (F 20 (-10))
-- = F (-1) 1
-- negate  (F 20 2)
-- = F (-20) 2

-- OU ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--instance Num Frac where
{--
    (F a b) + (F c d) | b == d = normaliza $ F (a + c) b
                      | otherwise = normaliza $ F (a * d + b * c) (b * d)
    x - y = x + negate y
    (F a b) * (F c d) = F (a * c) (b * d)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a == 0 = 0
                   | a * b > 0 = 1
                   | otherwise = (-1)
    fromInteger x = F x 1
--}
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

dobrofracao :: Frac -> [Frac] -> [Frac]
dobrofracao x [] = []
dobrofracao x l = filter (> 2 * x) l
-- ou dobrofracao x l = filter (\x -> f * (fromIntefer 2) > x) l
-- dobrofracao (F 20 10) [(F 2 1),(F 8 2),(F 16 2),(F 30 10)]
-- = [F 16 2]

-- Exercício 2 

data Exp a = Const a | Simetrico (Exp a) | Mais (Exp a) (Exp a) | Menos (Exp a) (Exp a) | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
    show (Const a) = show a  -- aparece erro se nao tivesse colocado Show a => Show Exp a (!!!!)
    show (Simetrico a) = "(-" ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ "+" ++ show b ++")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

{--
(!!!!) -> * No instance for (Show a) arising from a use of `show'
      Possible fix:
        add (Show a) to the context of the instance declaration
    * In the expression: show a
      In an equation for `show': show (Const a) = show a
      In the instance declaration for `Show (Exp a)'
--}

--  show (Const 2)
-- = "2"
--show (Mult (Const 2) (Menos (Const 3) (Const 2)))
-- = "(2 * (3 - 2))"



instance (Num a,Eq a) => Eq (Exp a) where
    x == y = valorDe x == (valorDe y)

valorDe :: (Num a) => Exp a -> a
valorDe (Const a) = a
valorDe (Simetrico a) = - (valorDe a)
valorDe (Mais a b) = valorDe a + valorDe b
valorDe (Menos a b) = valorDe a - valorDe b
valorDe (Mult a b) = valorDe a * valorDe b


instance (Num a, Eq a) => Num (Exp a) where
    x + y = Const (valorDe x + valorDe y)
    --
    x - y = Const (valorDe x - valorDe y)
    --
    x * y = Const (valorDe x * valorDe y)
    --
    negate (Const a) = Const (- a)
    negate (Simetrico a) = a
    negate (Mais a b) = Mais (- a) (- b)
    negate (Menos a b) = Menos b a
    negate (Mult a b) = Mult (-a) b
    --
    fromInteger x = Const (fromInteger x)
    --
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs (Mais a b) = abs (a + b)
    abs (Menos a b) = abs (a - b)
    abs (Mult a b) = abs (a * b)
    --
    signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
    signum (Simetrico a) = - signum a
    signum (Mais a b) = Const (if abs (a + b) == a + b then if a + b == 0 then 0 else 1 else (-1))
    signum (Menos a b) = Const (if abs (a - b) == a - b then if a - b == 0 then 0 else 1 else (-1))
    signum (Mult a b) = Const (if abs (a * b) == a * b then if a * b == 0 then 0 else 1 else (-1))

-- copiei tudo da alinea anterior por outra pessoa porque nao entendi ainda "(Num a,Eq a) => Eq (Exp a)  em vez so de  NUm a ou Exp a ou o que for e ter que meter algo antes"

-- Exercício 3

data Movimento = Credito Float | Debito Float   deriving Show
data Data = D Int Int Int  deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]  --deriving Show

instance Ord Data where
    compare (D d m a) (D d' m' a') | (a < a') || (a == a' && m<m') || (a == a' && m == m' && d < d') = LT                      -- temos de deriving Eq para poder comparar as datas e ver a sua Ord
                                   | (a == a' && m == m' && d == d') = EQ
                                   | otherwise = GT
-- compare (D 25 09 2002) (D 1 1 2021)
-- = LT
-- compare (D 25 09 2002) (D 25 09 2002)
-- = EQ
-- compare (D 25 09 2002) (D 1 1 2000)
-- = GT


instance Show Data where
  -- show (D d m a) = "dia" ++ " " ++ show d ++" " ++ "do mes" ++ " "++ show m ++ " " ++ "do ano" ++ " " ++ show a
-- show (D 25 09 2002)
-- = "dia 25 do mes 9 do ano 2002"
 -- show (D d m a) = concat $ intersperse "/" $ map (show) [d,m,a]
-- show (D 25 09 2002)
-- = "25/9/2002"
   show (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a


--ordena2 :: Extracto -> Extracto -- aqui a funcao tem apenas um argumento nao podemos ir por recursividade a comparar os extratos
--ordena2 (Ext x l) = (Ext x (sortBy (\(d,_,_) (d',_,_) -> compare d d') l))   -- nao posso usar apenas sort porque aqui apenas usamos um argumento e a funcao sorte necessita de dois
                                                                          -- logo vou usar a sortBy que apenas necessita de um

-- vamos ter que deriving show para ver o extrato e nos movientos para poder ver o extrato
ordenaaa :: Extracto -> Extracto
ordenaaa (Ext a l) = let l2 = sortOn (\(x,y,z) -> x) l
                   in (Ext a l2)

-- ordenaaa (Ext 1000 [(D 15 09 2000,"Jose",Debito 150),(D 10 09 2000,"Maria",Credito 230),(D 20 11 2010,"Almeida",Debito 250),(D 20 11 2020,"Quim", Debito 100)])
-- Ext 1000.0 [(10/9/2000,"Maria",Credito 230.0),(15/9/2000,"Jose",Debito 150.0),(20/11/2010,"Almeida",Debito 250.0),(20/11/2020,"Quim",Debito 100.0)]



instance Show Extracto where 
    show (Ext n l) = "Saldo anterior:" ++ show n ++
                     "\n-------------------------------------------------" ++
                     "\nData           Descricao        Credito    Debito" ++
                     "\n-------------------------------------------------\n" ++ concatMap (\(dat,str,mov) -> show dat ++ replicate (11- (length (show dat ))) ' ' ++ map (toUpper) str ++ "    \n") l ++
                     "--------------------------------------------------" ++
                     "\nSaldo actual:" ++ show (Ext n l)
-- nesta tiramos o deriving show que colocamos porque é o que estamos a definir
-- deu um loop infinito rever isto depois

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> (acc + n)
                                                        Debito n -> (acc - n)) x lm
