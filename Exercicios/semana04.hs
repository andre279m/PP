
halve :: [a] -> ([a],[a])
halve xs = (ys,zs)
    where ys = take (length xs `div` 2) xs
          zs = drop (length xs `div` 2) xs

halve' :: [a] -> ([a],[a])
halve' xs =
    let meio = length xs `div` 2
    in (take meio xs, drop meio xs)

raizes :: Double -> Double -> Double -> (Double,Double)
raizes a b c = ( (-b + d)/ (2*a),(-b - d)/ (2*a))
    where d =sqrt(b^2 - 4 *a *c)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximo xs

substitui :: Eq a =>a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui z y (x:xs) 
    | x == z = y:substitui  z y xs
    |otherwise =  x:substitui z y xs

posicoes :: [Int] -> Int -> [Int]
posicoes [] _ = []
posicoes xs 0 = []
posicoes xs 1 = xs
posicoes zs x = [y | y <- [0..length zs -1], z <- zs, z `mod` x == 0]

repBinaria :: Int -> Integer
repBinaria x
    | x < 2 = x
    |otherwise = x 
