{-
    A.
-}

frequencias :: String -> [Int]
frequencias "" = []
frequencias xs = [contaChar x xs | x <- xs]

contaChar :: Char -> String -> Int
contaChar _ [] = 0
contaChar y xs = length ys
    where ys = [ w | w <- xs, w == y]

{-
    B.
-}

abc :: [Char]
abc = ['a' .. 'z']

vogais :: [Char]
vogais = [ 'a', 'e', 'i', 'o', 'u', 'y']

pequenasPalavras :: [String]
pequenasPalavras = [a : b : c : [] | a <- abc, b <- abc, c <- abc, (a `elem` vogais) || (b `elem` vogais) || (c `elem` vogais)]

{-
    C.
-}

legendaCampainha :: Int -> Int -> [(String, Int)] -> [String]
legendaCampainha n m ys 
    | n <= 0  = []
    | otherwise = [show x ++ fst y | x <- [1..n+1], x /= m, y <- ys, if n < m then x <= snd y else x <= snd y + 1]
