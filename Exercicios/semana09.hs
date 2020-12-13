writePrimes :: [Int] -> IO ()
writePrimes [] = return()
writePrimes (x:xs) = do 
        putStrLn $ formatarPrimo x
        writePrimes xs

formatarPrimo :: Int -> String
formatarPrimo x = (show x ++ "th prime is " ++ show (primes !! x))

primes :: [Integer]
primes = sieve [2..]
    where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]    


palindrome:: String -> Bool
palindrome xs = xs == reverse xs

isPalindrome ::IO()
isPalindrome = do
    putStrLn "Escreva uma frase:"
    xs <- getLine
    if null xs 
        then
            return ()
        else do
            putStrLn $ if palindrome xs
                     then
                        "Sim"
                     else
                        "Nao"
            isPalindrome


printEven :: Int -> IO()
printEven x = putStrLn $ if even x then "Par" else "Impar"

showParity :: [Int] -> IO()
showParity [] = return ()
showParity (x:xs) = do
    printEven x
    showParity xs

mapM_' :: (a -> IO b)-> [a] -> IO()
mapM_' _ [] = return ()
mapM_' f (x:xs) = do
    f x
    mapM_' f xs

showParity' :: [Int] -> IO()
showParity' = mapM_ printEven

somar :: IO()
somar = do 
    putStr "Quantos números ?"
    n <- getLine
    z <- getNumeros (read n)
    putStrLn $"A soma é " ++ show z
        

getNumeros :: Int -> IO Int
getNumeros 0 = return 0
getNumeros n = do
    i <- getNumeros $ n-1
    x <- getLine
    return $read x+i


getNumeros' :: Int -> Int -> IO()
getNumeros' 0 m = putStrLn $ show m
getNumeros' n m = do
    x <- getLine
    getNumeros' (n-1) $ read x + m 