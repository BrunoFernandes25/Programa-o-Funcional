import Data.List 

-- Exercício 1 

-- a)

--any :: (a -> Bool) -> [a] -> Bool
-- any odd [1..10] == True

any' :: (a -> Bool) -> [a] -> Bool 
any' f [] = False
any' f (h:t) = if f (h) then True else any' f (t)

-- b)

-- zipWith :: (a->b->c) -> [a] -> [b] -> [c]
-- zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44]

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (h:t) (h':t') = (f h h'): (zipWith' f t t')
zipWith' _ _ _ = []

-- takeWhile :: (a->Bool) -> [a] -> [a]
-- takeWhile odd [1,3,4,5,6,6] == [1,3]

-- c)

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) | (f h) == True = h:(takeWhile' f t)
                   | otherwise = []

-- d)

--dropwhile :: (a->Bool) -> [a] -> [a]
--dropwhile odd [1,3,4,5,6,6] == [4,5,6,6]

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile'f [] = []
dropWhile' f (h:t) | (f h) == True = (dropWhile' f t)
                   | otherwise = (h:t)

-- e) 

--span :: (a-> Bool) -> [a] -> ([a],[a])
--span p l = (takeWhile p l, dropWhile p l)

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h = (h:x,y)
              | otherwise = ([],h:t)
              where (x,y) = span' f t

--deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]-
--deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)] = [(3,3),(4,2)]

-- basicamente o que faz é ver o segundo elemento em x -> (1,2) e ver o segundo elemento em cada par da lista 
-- e se se corresponder a funcao dada por nos elimina esse elemento 
-- e retorna o resto da lista mesmo que os outros pares tambem satisfacam a condicao da nossa funcao

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f e (h:t) = if f e h
                then t 
                else h: deleteBy' f e t

--sortOn :: Ord b => (a -> b) -> [a] -> [a]
--sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]

--basicamente ordena a lista a partir do fsr do par

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = insert' f h (sortOn' f t)

insert' :: Ord b => (a -> b) -> a -> [a] -> [a]
insert' f x [] = [x]
insert' f x (h:t) | (f x) > (f h) = h: insert' f x t
                  | otherwise = (x:h:t) 

-- Exercício 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)


selgrau :: Int -> Polinomio -> Polinomio 
selgrau n l = filter (\x -> snd x == n) l  

selgrau2 :: Int -> Polinomio -> Polinomio 
selgrau2 n l = filter (\(a,b) -> b == n) l  


conta :: Int -> Polinomio -> Int 
conta n p = length $ filter (\x -> snd x == n) p
-- conta 2 [(1,0),(2,2),(5,3),(4,22),(1,2),(3,333)] = 2

grau :: Polinomio -> Int 
grau l = foldl (\acc x -> if acc > snd x then acc else snd x) 0 l
--grau  [(1,0),(2,2),(5,3),(4,22),(1,2),(3,333)] = 333

deriv :: Polinomio -> Polinomio
deriv l = filter (/= (0,0)) $ map (\(a,b) -> if (b > 0 || b < 0) then ((a * fromIntegral b),b-1) else (0,0)) l
-- deriv [(1,2),(2,1),(3,0)] = [(2.0,1),(2.0,0)]

calcula :: Float -> Polinomio -> Float
calcula x l = foldl (\acc (a,b) -> acc + a * x^b) 0 l 
-- calcula 2 [(1,2),(2,1),(3,0)] = 11.0

simp :: Polinomio -> Polinomio
simp n = filter (\x -> fst x /= 0) n
--simp [(0,1),(1,2),(3,0)] = [(1.0,2),(3.0,0)]

mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) l = map (\(x,y) -> (a*x,b+y)) l
-- mult (2,2) [(1,2),(3,4)] =  [(2.0,4),(6.0,6)]

ordena :: Polinomio -> Polinomio
ordena = sortOn' (snd ) 


normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((a,b):p) = (sum [x | (x,e) <- selgrau b p] + a,b): normaliza [(x',e') | (x',e') <- p, e' /= b] 
-- normaliza [(1,2),(3,2),(4,4),(8,1),(9,0)] = [(4.0,2),(4.0,4),(8.0,1),(9.0,0)]

{--
normaliza2 :: Polinomio -> Polinomio
normaliza2 [] = []
normaliza2 (h:t) = auxNormaliza h t

auxNormaliza :: Monomio -> Polinomio -> Polinomio
auxNormaliza a [] = []
auxNormaliza (a,b) ((c,d):t) | b == d = ((a+c),b):t
                             | otherwise = (c,d):(auxNormaliza (a,b) t)
--}

normaliza2 :: Polinomio -> Polinomio
normaliza2 [] = []
normaliza2 ((a,b):t) = let f1 = filter (\(_,b1) -> b == b1) ((a,b):t) 
                           f2 = filter (\(_,b1) -> b /= b1) ((a,b):t)
                           x = sum (map fst f1)
                        in (x,b): normaliza2 f2


soma :: Polinomio -> Polinomio -> Polinomio 
soma n p = normaliza $ (++) n p
-- soma [(1,2),(2,2),(4,4),(5,0)] [(2,0),(3,1),(3,4),(6,1)] = [(3.0,2),(7.0,4),(7.0,0),(9.0,1)]

--(++) :: [a] -> [a] -> [a]
--(+):: Num a => a -> a -> a

produto :: Polinomio -> Polinomio -> Polinomio 
produto n p = foldl (\acc (a,b) -> soma (mult (a,b) p) acc) [] n
--produto [(2,2),(3,3),(4,1)] [(1,2),(2,3)] = [(4.0,3),(10.0,4),(7.0,5),(6.0,6)]
-- ou 
produto' :: Polinomio -> Polinomio -> Polinomio 
produto' [] p = []
produto' (n:ns) p = normaliza $ (mult n p ++ produto ns p) 
-- ou 
produto'' :: Polinomio -> Polinomio -> Polinomio 
produto'' n p = normaliza $ (concat $ (map (\m -> mult m p) n))
-- a unica diferenca sao a ordem dos resultados mas da tudo o mesmo 

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)

-- Exercício 3 
-- ex: [[1,2,3], [0,4,5], [0,0,6]]  =   | 1 2 3 |
--                                      | 0 4 5 |
--                                      | 0 0 6 |

type Mat a = [[a]]

dimOk :: Mat a -> Bool 
dimOk (l:lt) = all (\x -> length x == length l) lt
-- dimOk [[1,2,3],[4,5,6],[7,8]] = False

dimMat :: Mat a -> (Int,Int)
dimMat (h:t) = (length (h:t), length h)
-- a dimensoa da matriz exemplo é igual a 3x3  (linhas x colunas) 
-- dimMat [[1,2,3],[4,5,6],[7,8,9],[10,11,12]] = (4,3)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] x = x
addMat (h:t) (h':t') | dimMat (h:t) == dimMat (h':t') = zipWith (+) h h' : addMat t t'
                     | otherwise = error "Impossivel somar matrizes com dimensoes que nao sao iguais !!!"
-- addMat [[1,2,3],[4,5,6],[7,8,9]] [[1,1,1],[1,1,1],[1,1,1]] = [[2,3,4],[5,6,7],[8,9,10]]

transpose' :: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m = (map head m): (transpose' (map tail m))
-- transpose' [[1,2,3],[4,5,6],[7,8,9]] = [1,4,7],[2,5,8],[3,6,9]]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat [] _ = []
multMat _ [] = []
multMat  m1 m2 = if length (head m1) == length m2 then multiplicaMAt m1 (transpose' m2) else error "Impossivel multiplicar matrizes com linhas e colunas diferentes respetivamente"

multiplicaMAt :: Num a => Mat a -> Mat a -> Mat a
multiplicaMAt [] m = []
multiplicaMAt m [] = []
multiplicaMAt (h:t) (x:xs) = (zipWith' (*) h x) : (multiplicaMAt t xs)
-- tenho de ver esta nao ta bem

-- a de baixo ja esta correta 
multMAtt :: Num a => Mat a -> Mat a -> Mat a
multMAtt [] _ = []
multMAtt _ [] = []
multMAtt m1 m2 = (map (\x -> (sum (zipWith' (*) (head m1) x ))) (transpose m2)) : multMAtt (tail m1) m2
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

zipWMat :: (a->b->c) -> Mat a -> Mat b -> Mat c 
zipWMat f m1 m2 = zipWith (\l1 l2 -> zipWith f l1 l2) m1 m2 
-- copiei esta funcao de cima 

--triSup :: Num a => Mat a -> Bool 
triSup :: (Eq a, Num a) => [[a]] -> Bool
triSup [] = True 
triSup (h:t) = (all (==0) x) && triSup z
    where
        x = map head t
        z = map tail t 
-- triSup [[1,2,3],[0,5,6],[0,0,1]] = True

rotateLeft :: Mat a -> Mat a
rotateLeft m = transpose (map reverse m)