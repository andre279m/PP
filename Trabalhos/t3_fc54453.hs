listaDeAlguidaresVazia :: [a]
listaDeAlguidaresVazia = []

adicionaAListaDeAlguidares :: Ord a => Int -> a -> [[a]] -> [[a]]
adicionaAListaDeAlguidares 0 _ _ = [[]]
adicionaAListaDeAlguidares _ m [] = [[m]]
adicionaAListaDeAlguidares _ m [[]] = [[m]]
adicionaAListaDeAlguidares n m [(x:xs)]
    | length (x:xs) < n =  [inserirLista m (x:xs)] 
    | otherwise = [init (inserirLista m (x:xs)), [last (inserirLista m (x:xs))]]
adicionaAListaDeAlguidares n m (x1:x2:xs) 
    | head x2 < m = x1 : adicionaAListaDeAlguidares n m (x2:xs)
    | length x1 < n = (inserirLista m x1):x2:xs
    | otherwise = init (inserirLista m x1) : adicionaAListaDeAlguidares n (last (inserirLista m x1)) (x2:xs)

inserirLista :: Ord a => a -> [a] -> [a]
inserirLista y [] = [y]
inserirLista y (x:xs)
    | y > x = x:(inserirLista y xs)
    | otherwise = y:x:xs

elemListaDeAlguidares :: Eq a => a -> [[a]] -> Bool
elemListaDeAlguidares m = foldl (\acc x -> if m `elem` x then True else acc) False

removerDaListaDeAlguidares :: Eq a => a -> [[a]] -> [[a]]
removerDaListaDeAlguidares _ [] = []
removerDaListaDeAlguidares m (x:xs) 
    | elemListaDeAlguidares m (x:xs) = removerDaLista m x : removerDaListaDeAlguidares m xs
    | otherwise = (x:xs)

removerDaLista :: Eq a => a -> [a] -> [a]
removerDaLista m xs = filter (/=m) xs

fromList :: Ord a => Int -> [a] -> [[a]]
fromList n = foldr (\x acc -> adicionaAListaDeAlguidares n x acc) listaDeAlguidaresVazia

mapListaDeAlguidares :: (Ord a, Ord b) => Int -> (a -> b) -> [[a]] -> [[b]]
mapListaDeAlguidares n f xs = fromList n (map f (concat xs))

createFastCache :: (Ord k,Ord v) => Int -> [k] -> [v] -> [[(k,v)]]
createFastCache n xs ys = fromList n (zip xs ys)

fastGet :: Eq k => [[(k,v)]] -> k -> [v]
fastGet xs k = foldl (\acc x -> if elemCache k xs then acc ++ (getLista k x) else acc) [] xs

getLista :: Eq k => k -> [(k,v)] -> [v]
getLista _ [] = []
getLista k' ((k,v):xs)
    | k' == k = v : getLista k' xs
    | otherwise = getLista k' xs

listaPrimeiros :: [(k,v)] -> [k]
listaPrimeiros xs = [k | (k,_) <- xs]

elemCache :: Eq k => k -> [[(k,v)]] -> Bool
elemCache k = foldl (\acc x -> if k `elem` (listaPrimeiros x) then True else acc) False