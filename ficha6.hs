

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

-- Exercício 1 

altura :: BTree a -> Int
altura Empty = 0 
altura (Node r e d) = 1 + (max (altura e) (altura d))   

-- altura ( Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))) )
-- = 4 

--             1 
--            / \
--           2   3
--              / \
--             4   5
--                / \
--               6   7

contaNodos :: BTree a -> Int
contaNodos Empty = 0 
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d
-- contaNodos ( Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))))
-- = 7

folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d 
-- folhas ( Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))))
-- = 4

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 (Node r e d) = Empty
prune a (Node r e d) = Node r (prune (a-1) e) (prune (a-1) d)
-- prune 3 ( Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty)))) = 
-- Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty))

--path :: [Bool] -> BTree a -> [a]
--path [] (Node r e d) = []
--path _ (Node r Empty Empty) = []
--path (h:t) (Node r (Node e e' d') (Node d e'' d'')) = case h of 
--                                                        False -> e : (path t (Node e e' d'))
--                                                        True  -> d : (path t (Node d e'' d''))
-- nao ta bem ver debaixo nova função !!!!
-- path [True,True,False] ( Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))))
-- = [3,5,6]

pathh :: [Bool] -> BTree a -> [a]
pathh _ Empty = []
pathh [] (Node r e d) = [r]
pathh (h:t) (Node r e d) = case h of
                True -> r : pathh (t) d
                False -> r : pathh (t) e


mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)
-- mirror ( Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))))
-- = Node 1 (Node 3 (Node 5 (Node 7 Empty Empty) (Node 6 Empty Empty)) (Node 4 Empty Empty)) (Node 2 Empty Empty)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r e d) (Node r' e' d') = Node (f r r') (zipWithBT f e e') (zipWithBT f d d')
zipWithBT _ _ _ = Empty
-- zipWithBT (+) (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)) (Node 4 (Node 5 Empty Empty) (Node 6 Empty Empty))
-- = Node 5 (Node 7 Empty Empty) (Node 9 Empty Empty)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = ((Node a e1 d1),(Node b e2 d2),(Node c e3 d3))
    where 
        (e1,e2,e3) = unzipBT e
        (d1,d2,d3) = unzipBT d

-- Exercício 2 
-- arvore binaria de procura exemplo 

--                            10 
--                          /    \
--                         5      15
--                        / \    / \
--                       3   7  12  20



minimo :: Ord a => BTree a -> a 
minimo (Node r Empty Empty) = r 
minimo (Node r e d) = minimo e 
-- minimo (Node 10 (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))) 
-- = 3

semMinimo :: Ord a => BTree a -> BTree a 
semMinimo Empty = Empty
semMinimo (Node _ Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d
--  semMinimo (Node 10 (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty)))
-- =  Node 10 (Node 5 Empty (Node 7 Empty Empty)) (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin Empty = error "nao minimo nem arvore para minSMin Empty"
minSmin (Node r Empty _) = (r,Empty)
minSmin (Node r e d) = (x,Node r y d)
    where (x,y) = minSmin e
-- minSmin (Node 10 (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))) 
-- = (3,Node 10 (Node 5 Empty (Node 7 Empty Empty)) (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty)))

minSminn :: Ord a => BTree a -> (a,BTree a)
minSminn Empty = error "nao tem minimo nem arvore para minSminn Empty"
minSminn (Node r Empty _) = (r,Empty)
minSminn (Node r e d) = (minimo e,Node r (semMinimo e) d)



remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d) | x < r = Node r (remove x e) d
                      | x > r = Node r e (remove x d)
                      | otherwise = removeAux x (Node r' e d')
    where removeAux p (Node r e d) = case e of Empty -> d 
                                               otherwise -> case d of Empty -> e
                                                                      otherwise -> Node r' e d'
          (r',d') = minSmin d

-- remove 10 (Node 10 (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))) 
-- = Node 12 (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) (Node 15 Empty (Node 20 Empty Empty))

-- Exercício 3 

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)


--                                          (10,"Antonio",ORD, Aprov 20)
--                                          /                          \
--                    (5,"Quim",TE,Aprov 12)                          (15,"Manel",MEL,Rep)
--                    /                   \                           /                  \
--        (2,"Ana",ORD,Faltou) (7,"Bruno",MEL,Aprov 17)  (12,"Ricardo",TE,Aprov 20)   (20,"Tiago",ORD, Aprov 15)


-- (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Faltou) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE,Aprov 20) Empty Empty) (Node (20,"Tiago",ORD, Aprov 15) Empty Empty)))



inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) | n > num = inscNum n d
                                 | n < num = inscNum n e   
                                 | otherwise = True 
--inscNum 10 (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Faltou) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE) 
-- = True

inscNome :: Nome -> Turma -> Bool 
inscNome n Empty = False 
inscNome n (Node (_,nome,_,_) e d) | n == nome = True
                                   | otherwise = inscNome n e || inscNome n d
-- inscNome "Ana" (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Faltou) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE,Aprov 20) Empty Empty) (Node (20,"Tiago",ORD, Aprov 15) Empty Empty)))
-- = True


trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,reg,_) e d) = case reg of 
                                        TE -> [(num,nome)] ++ trabEst e ++ trabEst d
                                        otherwise -> trabEst e ++ trabEst d
-- trabEst (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Faltou) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE,)
-- = [(5,"Quim"),(12,"Ricardo")]

nota :: Numero -> Turma -> Maybe Classificacao
nota x Empty = Nothing
nota x (Node (num,nome,_,clas) e d) | x == num = Just clas
                                    | x < num = nota x e 
                                    | otherwise = nota x d
-- ou nota _ _ = Nothing

-- nota 7 (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Faltou) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE,A)
-- = Just (Aprov 17)


percFaltas :: Turma -> Float 
percFaltas Empty = 0
percFaltas turma = somaFaltas turma / numerodeAlunos turma * 100
    where
        somaFaltas Empty = 0
        somaFaltas (Node (_,_,_,clas) e d) = case clas of Faltou -> 1 + somaFaltas e + somaFaltas d
                                                          otherwise -> somaFaltas e + somaFaltas d
        numerodeAlunos Empty = 0
        numerodeAlunos (Node (_,_,_,clas) e d) = 1 + numerodeAlunos e + numerodeAlunos d
-- percFaltas (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Faltou) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE,Aprov 20) Empty Empty) (Node (20,"Tiago",ORD, Aprov 15) Empty Empty)))
-- = 14.285715

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mediaAprov :: Turma -> Float
mediaAprov Empty = 0 
mediaAprov turma = somanotaAprov turma / numerodeAprov turma
    where 
        somanotaAprov Empty = 0
        somanotaAprov (Node (_,_,_,classific) e d) = case classific of Aprov x -> fromIntegral x + somanotaAprov e + somanotaAprov d
                                                                       otherwise -> somanotaAprov e + somanotaAprov d
        numerodeAprov Empty = 0
        numerodeAprov (Node (_,_,_,clas) e d) = case clas of 
                                                Aprov x -> 1 + numerodeAprov e + numerodeAprov d
                                                otherwise -> numerodeAprov e + numerodeAprov d 

        


-- mediaAprov (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Faltou) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE,Aprov 20) Empty Empty) (Node (20,"Tiago",ORD, Aprov 15) Empty Empty)))
-- = 16.8

--------------------------------------------------------------------------------------- estas duas de baixo fazem a media de aprovados na turma toda com os REP e com os Faltou
mediaAprov' :: Turma -> Float
mediaAprov' Empty = 0
mediaAprov' a = (soma a) / fromIntegral (contaNodos a)
  where soma :: Turma -> Float
        soma Empty = 0
        soma (Node (_,_,_,Aprov x) e d) = (fromIntegral x) + soma e + soma d
        soma (Node (_,_,_,_) e d) = soma e + soma d

--------------------------------------------------------------------------------------
mediaAprova :: Turma -> Float
mediaAprova Empty = 0
mediaAprova (Node a e d) = fst(x)/snd(x)
    where x= mediaAprovaux (Node a e d)

mediaAprovaux ::Turma -> (Float,Float)
mediaAprovaux (Empty)= (0,0)
mediaAprovaux (Node (_,_,_, Aprov l) e d)= (a+p,b+1)
    where p = fromIntegral(l)
          (a,b) = somaXPTO (mediaAprovaux (e)) (mediaAprovaux (d))
mediaAprovaux (Node _ e d) = (a,b+1)
    where (a,b) = somaXPTO (mediaAprovaux (e)) (mediaAprovaux (d))

somaXPTO ::(Num a, Num b) =>(a,b) -> (a,b) -> (a,b)
somaXPTO (a,b) (c,d) = ((a+c),(b+d))

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


aprovAv :: Turma -> Float
aprovAv Empty = 0 
aprovAv (Node (_,_,_,clas) e d) = case clas of 
                                Aprov x -> (1 + aprovAv e + aprovAv d) / (1 + aprovAv e + aprovAv d)
                                Rep -> (aprovAv e + aprovAv d) / (1+ aprovAv e + aprovAv d)
                                otherwise -> (aprovAv e + aprovAv d) / (aprovAv e + aprovAv d)
--  aprovAv (Node (10,"Antonio",ORD, Aprov 20) (Node (5,"Quim",TE,Aprov 12) (Node (2,"Ana",ORD,Aprov 20) Empty Empty) (Node (7,"Bruno",MEL,Aprov 17) Empty Empty)) (Node (15,"Manel",MEL,Rep) (Node (12,"Ricardo",TE,Aprov 20) Empty Empty) (Node (20,"Tiago",ORD,Rep) Empty Empty)))
-- = 1.0
-- nao sei bem o que é isto do racio copiei apenas tenho de pesquisar sobre isto
