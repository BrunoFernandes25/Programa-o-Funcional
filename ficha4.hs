import Data.Char

--Listas por compreensão e optimização com tupling e acumuladores

-- Exercício 1 

-- a)

fun1 :: [Int]
fun1 = [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]

fun1' :: Int -> [Int]
fun1' x | x <= 20 && x >= 1 = if mod x 6 == 0 then x: fun1' (x+1) else fun1' (x+1) 
        | otherwise = []
-- [6,12,18]

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- b)

-- queremos uma lista x com os elementos ( [y | y <- [1..20], mod y 2 == 0 ] ) e depois com esses elementos vemos quais sao multiplos de 3
-- ou seja em [1..20] iremos ter como elementos para verificar multiplos de 3 o [2,4,6,8,10,12,14,16,18,20] e a resposta final será
-- fun2' 1 = [6,12,18]

fun2 :: [Int]
fun2 = [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]

fun2' :: Int -> [Int]
fun2' y | y <= 20 && y >= 1 = if mod y 2 == 0 
                        then if mod y 3 == 0 
                        then y: fun2' (y+1) else fun2' (y+1) 
                        else fun2' (y+1)
        | otherwise = []
-- basicamente isto faz : se y for multiplo de 2 entao averigua se é de 3 tambem se nao for passa para o numero seguinte e ve se é multiplo de 2
-- quando é de 2 e de 3 entao fica esse y seguido de averiguar o mesmo ao resto da funcao senao passa para o elemento seguinte
-- [6,12,18]

--------------------------------------------------------------------------------------------------------------------------------------------------------

--c)
fun3 :: [(Int,Int)]
fun3 = [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]

-- aqui vamos ter que olhar para um certo x entre [0..20] e um y no mesmo intervalo e ver quais os numeros que somados dao igual a 30 e colocar
-- como resultado os x à esquerda e os y à direita.
-- fun3' 0 = [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

--fun3' :: Int -> Int -> [Int]
--fun3' x y | (x <= 20 && x >= 0) && (y <= 20 && y >=0) = if (sum x y == 30) then (x,y):fun3' (x+1,y-1) else fun3' (x+1,y+1)
--          | otherwise = []

--d)

fun4 :: [Int]
fun4 = [sum [y | y <- [1..x], odd y] | x <- [1..10]]

-- nesta funcao temos de ver 
-- [1,1,4,4,9,9,16,16,25,25]

-- Exercício 2 

-- a)  [1,2,4,8,16,32,64,128,256,512,1024]
lista1 :: [Int]
lista1 = [ 2^x | x <- [0..10] ] 

--b) [(1,5),(2,4),(3,3),(4,2),(5,1)]
lista2 :: [(Int,Int)]
lista2 = [ (x,(6-x)) | x <- [1..5] ]
--ou
lista2' :: [(Int,Int)]
lista2' = [ (x,y) | x <- [1..5],y <- [1..5], x + y == 6]

-- c) [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

lista3 :: [[Int]]
lista3 = [ [x | x <- [1 .. y]] | y <- [1..5] ]

-- d) [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]

lista4 :: [[Int]]
lista4 = [ [x^0 | x <- [1..y]] | y <- [1..5] ]

-- e) [1,2,6,24,120,720]
-- n*1, n*2,n*3 ... 

lista5 :: [Int]
lista5 = [ product [x | x <- [1..y]] | y <- [1..6] ]

-- Exercício 3
-- so pode percorrer a funcao uma vez, ou seja , tem de ser direto 

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (h:x,y)
                 | isAlpha h = (x,h:y)
                 | otherwise = (x,y)
                 where (x,y) = digitAlpha t

-- Exercício 4
-- so pode percorrer a funcao uma vez 

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (a+1,b,c)
          | h > 0 = (a,b,c+1)
          | otherwise = (a,b+1,c)
          where (a,b,c) = nzp t

-- Exercício 5 

-- (1) se dividirmos 6 2 = (3,0)   x>0 & y>0
-- (2) se dividirmos -6 2 = (-3,0) x<0 & y> 0 
-- (3) se dividirmos 6 -2 = (-3,0) x>0 & y< 0
-- (4) se dividirmos 0 2 = (0,0) x<0 & y> 0
-- (5) se dividirmos -6 0 = Impossivel da erro 
-- (6) se dividirmps 1 2 = (0,1)
-- (7) se dividirmos -6 -2 = (3,0) 
-- ja nao preciso destes casos particulares
-- rever funcao agora correta -----------------------------------------------------------------------------------------------------------------
divMod' :: Integral a => a -> a -> (a, a)
divMod' x y | y == 0 = error "Divided By Zero"
            | otherwise = divMod_aux x y 0
            where
                divMod_aux x y n | abs x - abs y >= 0 && ((x > 0 && ´nm
                	,
                	y > 0) || (x < 0 && y < 0)) = divMod_aux (x-y) y (n+1) 
                                 | ((x < 0 && y > 0) && (x + y) <= 0) || ((x > 0 && y < 0) && (x + y) >= 0) = divMod_aux (x-(-y)) y (n-1)
                                 | otherwise = (n, x)
------------------------------------------------------------------------------------------------------------------------------------------------

-- Exercício 6 

--fromDigits [1,2,3,4] = 1 * 10^3 + 2 * 10^2 + 3 * 10^1 + 4 * 10^0
-- = 4 + 10 * (3 + 10 * (2 + 10 * (1 + 10 * 0)))
-- queremos obter a linha de cima, logo acumulamos tudo e so depois multiplicamos pelo ultimo elemento 

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigits' :: [Int] -> Int
fromDigits' = foldl (\acc x -> x+10*acc) 0

-- Exercício 7

--maxSumInit :: (Num a, Ord a) => [a] -> a
--maxSumInit l = maximum [ sum [m | m <- init l] ]

-- basicamente soma o init de uma lista com a tail 
-- posso fazer um acc com init e somar a tail

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (h:t) = max' t h h

max' [] _ max = max
max' (h:t) total max
    |max>(total+h) = max' t (total+h) max
    |otherwise = max' t (total+h) (total+h)

-- maxSumInit [1,2,3] = 6

-- max' [1,2,3] 0 3


-- Exercício 8 

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibss :: Int -> Int
fibss n = acc n (0,1)
    where 
        acc 0 (a,c) = a
        acc 1 (a,c) = c
        acc n (a,c) = acc (n-1) (c,a+c)

-- sequencia de fibonacci -> 0 1 1 2 3 5 8 13 21

--fibss 2 = acc 2 (0,1) 
--        = acc (2-1) (1,0+1)
--        = acc 1 (1,1)
--        = 1

-- fibss 6 = acc 6 (0,1)
--           = acc 5 (1,1)
--           = acc 4 (1,2)
--           = acc 3 (2,3)
--           = acc 2 (3,5)
--           = acc 1 (5,8)
--           = acc 0 (8,13)
--           = 8
