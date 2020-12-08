module Eleicoes ( Candidato (A,B), Estado(..), Nacao, criaNacao, obterEstado, adicionaVotosEstado, adicionaVotosNacao, vencedorEstado, vencedorEleicao) where

data Candidato = A | B deriving (Eq, Show)

data Estado = Estado{nome :: String,
                     peso :: Int,
                     votosA :: Int,
                     votosB :: Int}

type Nacao = [Estado]

criaNacao :: [(String,Int)] -> Nacao
criaNacao [] = []
criaNacao ((x1,x2):xs) = Estado x1 x2 0 0 : criaNacao xs

obterEstado :: Nacao -> String -> Estado
obterEstado [] _ = Estado "" 0 0 0
obterEstado (x:xs) y 
    | nome x == y = x
    | otherwise = obterEstado xs y

adicionaVotosEstado :: Estado -> Int -> Int -> Estado
adicionaVotosEstado state xA xB = Estado (nome state) (peso state) (votosA state + xA) (votosB state + xB)  

adicionaVotosNacao :: Nacao -> [(String, Int, Int)] -> Nacao
adicionaVotosNacao xs ys = foldr (\x acc -> adicionaVotosNacao' x ys : acc ) [] xs

adicionaVotosNacao' :: Estado -> [(String, Int, Int)] -> Estado
adicionaVotosNacao' x [] = x
adicionaVotosNacao' x ((y1,y2,y3):ys) 
    | nome x == y1 = adicionaVotosNacao' (adicionaVotosEstado x y2 y3) ys
    | otherwise = adicionaVotosNacao' x ys

vencedorEstado :: Estado -> Maybe Candidato
vencedorEstado x 
    | votosA x > votosB x = Just A
    | votosA x < votosB x = Just B
    | otherwise = Nothing

vencedorEleicao :: Nacao -> Maybe Candidato
vencedorEleicao xs = vencedorEleicao' xs 0

vencedorEleicao' :: Nacao -> Int -> Maybe Candidato
vencedorEleicao' [] y 
    | y > 0 = Just A
    | y < 0 = Just B
    |otherwise = Nothing
vencedorEleicao' (x:xs) y
    | vencedorEstado x == (Just A) = vencedorEleicao' xs $ y + peso x
    | vencedorEstado x == (Just B) = vencedorEleicao' xs $ y - peso x
    | vencedorEstado x == Nothing = vencedorEleicao' xs y

instance Eq Estado where
    x == y = peso x == peso y && vencedorEstado x == vencedorEstado y

instance Show Estado where
    show x = nome x ++ " " ++ show (peso x) ++ " " ++ show (votosA x) ++ " " ++ show (votosB x)