

data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt 

calcula :: ExpInt -> Int
calcula x = case x of
    Const x -> x
    Simetrico x -> (- calcula x)
    Mais x y -> calcula x + calcula y
    Menos x y -> calcula x - calcula y
    Mult x y -> calcula x * calcula y
--  calcula (Mais (Const 5) (Const 5))
-- = 10
--  calcula (Simetrico (Const 5))
-- = -5
--  calcula (Mult (Const 5) (Const 4))
-- = 20

infixa :: ExpInt -> String
infixa x = case x of  
        Const x -> show x -- se fizesse "x" iria aparecer x em vez do numero 
        Simetrico x -> "-(" ++ infixa x ++ ")" 
        Mais x y -> "(" ++ infixa x ++ "+" ++ infixa y ++ ")" -- se pusesse '(' :infixa -... assim preferia antes "(" ++ ...
        Menos x y -> "(" ++ infixa x ++ "-" ++ infixa y ++ ")"
        Mult x y -> "(" ++ infixa x ++ "*" ++ infixa y ++ ")"
-- infixa (Simetrico (Const (-5)))
-- = "-(-5)"
-- infixa (Mais (Const 5) (Mult (Const 10) (Const 2)))
-- = "(5+(10*2))"

posfixa :: ExpInt -> String 
posfixa x = case x of 
        Const x -> show x
        Simetrico x -> posfixa x ++ "-"
        Mais x y -> posfixa x ++ " " ++ posfixa y ++ " +"
        Menos x y -> posfixa x ++ " " ++ posfixa y ++ " -"
        Mult x y -> posfixa x ++ " " ++ posfixa y ++ " *"
-- posfixa (Mais (Const 1) (Const 2))
-- = "12+"

-- Exercício 2

data RTree a = R a [RTree a] deriving Show

 
--                       10 
--                     / | \
--                    5  3  2
--                   / \
--                  12 13

-- R 10 [R 5 [R 12 [],R 13 []], R 3 [], R 2 []]

soma :: Num a => RTree a -> a
soma (R a l) = a + sum (map soma l)
-- soma (R 10 [R 5 [R 12 [],R 13 []], R 3 [], R 2 []])
-- = 45

altura :: RTree a -> Int 
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l)
-- altura (R 10 [R 5 [R 12 [],R 13 []], R 3 [], R 2 []])
-- = 3

prune :: Int -> RTree a -> RTree a
prune 1 (R a l) =  R a []
prune n (R a l) = R a (map (prune (n-1)) l) -- adicionar deriving show neste caso
--  prune 2 (R 10 [R 5 [R 12 [],R 13 []], R 3 [], R 2 []])
-- = R 10 [R 5 [],R 3 [],R 2 []]

mirror :: RTree a -> RTree a
mirror (R a []) = R a []
mirror (R a l) = R a (map mirror (reverse l))
--  mirror (R 10 [R 5 [R 12 [],R 13 []], R 3 [], R 2 []])
-- = R 10 [R 2 [],R 3 [],R 5 [R 13 [],R 12 []]]

postorder :: RTree a -> [a]  -- em vez de R a l faz a l R
postorder (R a []) = [a]
postorder (R a l) = (concat (map postorder l)) ++ [a]                 -- rever esta por causa do concatMap mas esta certa
-- postorder (R 10 [R 5 [R 12 [],R 13 []], R 3 [], R 2 []])
-- = [12,13,5,3,2,10]

-- Exercício 3 

--data BTree a = Empty | Node a (BTree a) (BTree a) 
data LTree a = Tip a | Fork (LTree a) (LTree a)

--         . -> Fork
--        / \
--       .   5
--      / \
--     7  10

-- (Fork (Fork (Tip 7) (Tip 10)) (Tip 5))

ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n
ltSum (Fork e d) = ltSum e + ltSum d
-- ltSum (Fork (Fork (Tip 7) (Tip 10)) (Tip 5))
-- = 22

listaLT :: LTree a -> [a]           -- EDR
listaLT (Tip n) = [n] 
listaLT (Fork e d) = listaLT e ++ listaLT d
-- listaLT (Fork (Fork (Tip 7) (Tip 10)) (Tip 5))
-- = [7,10,5]

ltHeight :: LTree a -> Int
ltHeight (Tip n) = 1 
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d) 
-- ltHeight (Fork (Fork (Tip 7) (Tip 10)) (Tip 5))
-- = 3

-- Exercício 4 

data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show
data BTreee a = Emptyy | Nodee a (BTreee a) (BTreee a) deriving Show 
data LTreee a = Tipp a | Forkk (LTreee a) (LTreee a) deriving Show

--           No 'b'
--          /     \
--       No 'c'   Leaf 4
--       /   \       
--  Leaf 1  Leaf 2

splitFTree :: FTree a b -> (BTreee a, LTreee b)
splitFTree (Leaf b) = (Emptyy, Tipp b)
splitFTree (No r e d) = (Nodee r x y, Forkk x' y') 
    where 
        (x,x') = splitFTree e
        (y,y') = splitFTree d

-- esta funcao fica     (Node 'b',  Fork )
--                        /  \       /   \
--                  Node 'c' []    Fork  Tip 4
--                   /  \           /
--                  []  []        Fork
--                                 /  \
--                             Tip 1  Tip 2

-- splitFTree (No 'b' (No 'c' (Leaf 1) (Leaf 2)) (Leaf 4))
-- = (Nodee 'b' (Nodee 'c' Emptyy Emptyy) Emptyy,Forkk (Forkk (Tipp 1) (Tipp 2)) (Tipp 4))

joinTrees :: BTreee a -> LTreee b -> Maybe (FTree a b)
joinTrees (Emptyy) (Tipp b) = Just (Leaf b)
joinTrees (Nodee r e d) (Forkk a b) = Just (No r e' d')
    where Just e' = joinTrees e a 
          Just d' = joinTrees d b 
joinTrees _ _ = Nothing   -- colocar deriving show no FTree ..
-- joinTrees (Nodee 'b' (Nodee 'c' Emptyy Emptyy) Emptyy) (Forkk (Forkk (Tipp 1) (Tipp 2)) (Tipp 4))
-- = Just (No 'b' (No 'c' (Leaf 1) (Leaf 2)) (Leaf 4))