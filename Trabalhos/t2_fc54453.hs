
paresConsecutivos :: [a] -> [(a, a)]
paresConsecutivos [] = []
paresConsecutivos [_] = []
paresConsecutivos (x:xs) = (x, head xs) : paresConsecutivos xs

diferencasConsecutivas :: [Int] -> [Int]
diferencasConsecutivas [] = []
diferencasConsecutivas [_] = []
diferencasConsecutivas (x:xs) = head xs - x : diferencasConsecutivas xs

claramentePior :: (String, Int, Int) -> [(String, Int, Int)] -> Bool
claramentePior _ [] = False
claramentePior (j1,j2,j3) ((_,x2,x3):xs)
    | j2 < x2 && j3 < x3 = True
    | otherwise = claramentePior (j1,j2,j3) xs

filtroJogadores :: [(String, Int, Int)] -> [(String, Int, Int)]
filtroJogadores [] = []
filtroJogadores xs = filtroJogadores' xs xs

filtroJogadores' :: [(String, Int, Int)] -> [(String, Int, Int)] -> [(String, Int, Int)]
filtroJogadores' [] _ = []
filtroJogadores' _ [] = []
filtroJogadores' (x:xs) ys
    | not (claramentePior x ys) = x : filtroJogadores' xs ys
    | otherwise = filtroJogadores' xs ys

preencherVazio :: [[Int]] -> Int
preencherVazio [] = 0
preencherVazio (x:xs)
    | 0 `elem` x = qualNumero x
    | otherwise  = preencherVazio xs

qualNumero :: [Int] -> Int
qualNumero [] = 0
qualNumero (x:xs) 
    | not (1 `elem` (x:xs)) = 1
    | not (2 `elem` (x:xs)) = 2
    | not (3 `elem` (x:xs)) = 3
    | not (4 `elem` (x:xs)) = 4
    | not (5 `elem` (x:xs)) = 5
    | not (6 `elem` (x:xs)) = 6
    | not (7 `elem` (x:xs)) = 7
    | not (8 `elem` (x:xs)) = 8
    | not (9 `elem` (x:xs)) = 9
    | otherwise             = 0