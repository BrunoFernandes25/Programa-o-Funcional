-- Recursividade

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a == b = [a]
               | a < b = a: enumFromTo' (a+1) b
               | otherwise = []

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c 
    | a < b && a > c = []
    | a > b && a < c = []
    | otherwise = a: enumFromThenTo' b ((b-a)+b) c

--(++) :: [a] -> [a] -> [a]
maismais :: [a] -> [a] -> [a]
maismais l [] = l 
maismais [] l = l
maismais (h:t) l = h: maismais t l

-- (!!) :: [a] -> Int -> a 
exclamacao :: [a] -> Int -> a 
exclamacao (h:t) 0 = h 
exclamacao (h:t) n = exclamacao t (n-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse t ++ [h]

take' :: Int -> [a] -> [a]
take' x [] = []
take' 0 (h:t) = []
take' n (h:t) = h:( take' (n-1) t )

drop' :: Int -> [a] -> [a]
drop' x [] = []
drop' 0 (h:t) = h:t
drop' x (h:t) = drop' (x-1) t

zipp :: [a] -> [b] -> [(a,b)]
zipp [] l = []
zipp l [] = []
zipp (h:t) (h':t') = (h,h'): zipp t t'

elemm :: Eq a => a -> [a] -> Bool
elemm x [] = False 
elemm x (h:t) = if x == h then True else elemm x t

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x: replicate (n-1) x

intersperse' :: a -> [a] -> [a]
intersperse' n [] = []
intersperse' n [x] = [x]
intersperse' n (h:t) = h:n: intersperse' n t

------------------------------------------------------------------------------------------
group' :: Eq a => [a] -> [[a]]
group' [] = [[]]
group' [x] = [[x]]
group' (h:t) = iguais (h:t) : group' (diferentes (h:t))

iguais :: Eq a => [a] -> [a]
iguais [] = []
iguais (h:t:p) = if h == t then h: iguais (t:p) else [h]

diferentes :: Eq a => [a] -> [a]
diferentes [] = []
diferentes (h:t:p) = if h == t then diferentes (t:p) else (t:p)
------------------------------------------------------------------------------------------
concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [[x]] = [x]
concat' (h:t) = h ++ concat' t 

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l] 

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] l = True
isPrefixOf' l [] = False
isPrefixOf' (h:t) (h':t') = if h == h' then isPrefixOf' t t' else False 

isSufixOf' :: Eq a => [a] -> [a] -> Bool
isSufixOf' [] l = True 
isSufixOf' l [] = False 
isSufixOf' (h:t) (h':t') = if h == h' then False else isSufixOf' t t'

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] l = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (h:t) (h':t') = if h == h' then isSubsequenceOf' t t' else isSubsequenceOf' (h:t) t' 

---------------------------------------------------------------------------------------------------------------
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x [] = []
elemIndices' x (h:t) = elemAUX x (h:t) 0

elemAUX :: Eq a => a -> [a] -> Int -> [Int]
elemAUX x [] _ = []
elemAUX x (h:t) n = if x == h then n : elemAUX x t (n+1) else elemAUX x t (n+1)

---------------------------------------------------------------------------------------------------------------
nub' :: Eq a => [a] -> [a] 
nub' [] = []
nub' [x] = [x]
nub' (h:t) = if elemm2 h t then nub' t else h: nub' t

elemm2 :: Eq a => a -> [a] -> Bool
elemm2 x [] = False
elemm2 x (h:t) = if x == h then True else elemm2 x t
-- esta da de ordem aleatoria mas correta 
-- ou 
nubb :: Eq a => [a] -> [a]
nubb [] = []
nubb [x] = [x]
nubb (h:t) = h: nubb (nubbAUX h t)

nubbAUX :: Eq a => a -> [a] -> [a]
nubbAUX x [] = []
nubbAUX x (h:t) = if x == h then nubbAUX x t else h: nubbAUX x t
-- esta da de forma ordenada e correta é melhor esta!! 
-----------------------------------------------------------------------------------------------------

delete' :: Eq a => a -> [a] -> [a]
delete' x [] = []
delete' x (h:t) = if x == h then t else h: delete' x t

-- (\\) :: Eq a => [a] -> [a] -> [a]
barras :: Eq a => [a] -> [a] -> [a]
barras [] l = []
barras l [] = l 
barras l (h:t) = barras (remove h l) t

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (h:t) = if x == h then t else h: remove x t 

--------------------------------------------------------------------------------
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' l [] = l
union' l (h:t) = union'(adiciona h l) t

adiciona :: Eq a => a -> [a] -> [a]
adiciona x [] = [x]
adiciona x (h:t) = if x == h then (h:t) else h: adiciona x t

-- ou 
unionnn :: Eq a => [a] -> [a] -> [a]
unionnn l [] = l
unionnn l (h:t)
    | h `elem` l = unionnn l t
    | otherwise = unionnn (l ++ [h]) t
------------------------------------------------------------------------------

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' l [] = []
intersect' (h:t) l = if elem'' h l then h: intersect' t l else intersect' t l

elem'' :: Eq a => a -> [a] -> Bool 
elem'' x [] = False 
elem'' x (h:t) = if x == h then True else elem'' x t 
--------------------------------------------------------------------------------

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = []
insert' x (h:t) | x <= h = x:h:t
                |otherwise = h: insert' x t

unwords' :: [String] -> String 
unwords' [] = []
unwords' (h:t) = h ++ " " ++ unwords' t

unlines' :: [String] -> String 
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

----------------------------------------------------------------------------
pMaior :: Ord a => [a] -> Int 
pMaior [x] = 0 
pMaior (h:t) = if ( h == maximo (h:t) ) then 0 else 1 + pMaior t

maximo :: Ord a => [a] -> a 
maximo [x] = x
maximo (h:t:p) = if h < t then maximo (t:p) else maximo (h:p)
---------------------------------------------------------------------------------

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False 
temRepetidos [x] = False 
temRepetidos (h:t) = if repetido h t then True else temRepetidos t

repetido :: Eq a => a -> [a] -> Bool
repetido x [] = False 
repetido x (h:t) = if x == h then True else repetido x t 
----------------------------------------------------------------------------------

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | ( (h >= '0') && (h <= '9') ) = h: algarismos t
                 | otherwise = algarismos t

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (h:t:p) = t: posImpares p 

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (h:t:p) = h: posPares p

isSorted :: Ord a => [a] -> Bool 
isSorted [] = True 
isSorted [x] = True
isSorted (h:t:p) = if h <= t then isSorted (t:p) else False

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = (insert h) (iSort t)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) = if x <= h then x:h:t else h: insert x t

menor :: String -> String -> Bool
menor [] [] = False 
menor [] _ = True
menor _ [] = False 
menor (h:t) (h':t') | h < h' = True 
                    | h == h' = menor t t'
                    | otherwise = False 

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False 
elemMSet x ((h,p):t) = if x == h then True else elemMSet x t 

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet [(h,p)] = p
lengthMSet ((h,p):t) = p + lengthMSet t 

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((h,1):p) = h: converteMSet p
converteMSet ((h,p):t) = h:converteMSet ((h,p-1):t)

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((h,p):t) = if x == h then (h,p+1):t else (h,p):insereMSet x t

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((h,1):t) = if x == h then t else (h,1):removeMSet x t
removeMSet x ((h,p):t) = if x == h then (h,p-1):t else (h,p):removeMSet x t

----------------------------------------------------------------------------------------------------------------------------------------------------------
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet [h] = [(h,1)]
constroiMSet (h:t) = (h, repete h (h:t)):constroiMSet (remove2 h t)

repete :: Eq a => a -> [a] -> Int 
repete x [] = 0 
repete x (h:t) = if x == h then 1 + (repete x t) else 0 -- pus 0 porque a lista ta ordenada por isso se nao é igual ao h nao pode ser igual ao t

remove2 :: Eq a => a -> [a] -> [a]
remove2 x [] = []
remove2 x (h:t) = if x == h then remove2 x t else (h:t)
-----------------------------------------------------------------------------------------------------------------------------------------------------------

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' (h:t) = case h of 
            Left h -> (h:a,b)
            Right h -> (a,h:b)
        where (a,b) = partitionEithers' t

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just h):t) = h: catMaybes' t
catMaybes' ((Nothing):t) = catMaybes' t

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' (x,y) [] = (x,y)
posicao' (x,y) (Norte:t) = posicao' (x,y+1) t 
posicao' (x,y) (Sul:t) = posicao' (x,y-1) t 
posicao' (x,y) (Este:t) = posicao' (x+1,y) t 
posicao' (x,y) (Oeste:t) = posicao' (x-1,y) t

caminho' :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho' (x,y) (x',y') | x < x' = Este  : caminho' (x+1,y) (x',y')
                       | x > x' = Oeste : caminho' (x-1,y) (x',y')
                       | y < y' = Norte : caminho' (x,y+1) (x',y')
                       | y > y' = Sul   : caminho' (x,y-1) (x',y')
                       | otherwise = []

vertical' :: [Movimento] -> Bool
vertical' [] = True
vertical' (h:t) = case h of
                Norte -> vertical' t
                Sul -> vertical' t
                _ -> False

data Posicao = Pos Int Int deriving Show

maisCentrall :: [Posicao] -> Posicao 
maisCentrall [x] = x
maisCentrall ((Pos x y):(Pos x' y'):t) | dist (x,y) > dist (x',y') = maisCentrall ((Pos x' y'):t)
                                       | otherwise = maisCentrall ((Pos x y):t)

dist :: (Int,Int) -> Float
dist (x,y) = sqrt (fromIntegral ((y^2) + (x^2)))
-- distancia a origem logo é igual a (y-0)^2 + (x-o)^2

vizinhoss :: Posicao -> [Posicao] -> [Posicao]
vizinhoss x [] = []
vizinhoss (Pos x y) ((Pos x' y'):t) | (x == (x'-1)) || (x == (x' +1)) || (y == (y'-1)) || (y== (y'+1)) = (Pos x' y' ) : (vizinhoss (Pos x y) t)
                                    | otherwise = vizinhoss (Pos x y) t

mesmaOrdenadaa :: [Posicao] -> Bool
mesmaOrdenadaa [] = True
mesmaOrdenadaa [x] = True
mesmaOrdenadaa ((Pos x y):(Pos x' y'):t) | y == y' = mesmaOrdenadaa ((Pos x y):t) 
                                         | otherwise = False 

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOKK :: [Semaforo] -> Bool
interseccaoOKK l = (contaNaoVermelhos l) <= 1
    where contaNaoVermelhos :: [Semaforo] -> Int
          contaNaoVermelhos [] = 0
          contaNaoVermelhos (Vermelho:t) = contaNaoVermelhos t
          contaNaoVermelhos (_:t) = 1 + contaNaoVermelhos t