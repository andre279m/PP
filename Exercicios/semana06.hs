--11
mult = (\x y z -> x * y * z)

--12
maisMais = (\xs ys -> xs++ys)

maisUmDois = (\xs -> xs ++[1,2])

umDoisMais = (\xs -> [1,2] ++ xs)

--17
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' xs = foldr (\x acc -> x + acc) 0 xs 

--sum'' = foldr (+) 0

length' = foldl (\y _ -> y+1) 0

--18
map' f = foldr (\x acc -> f x : acc) []

