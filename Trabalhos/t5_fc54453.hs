import System.Environment
import Data.List

main :: IO ()
main = do
    (file:_) <- getArgs
    content <- readFile file
    printer content
    linhas <- lines content
    stackGrep linhas []
    

printer :: Show a => [a] -> IO ()
printer [] = return ()
printer (x:xs) = do
    putStrLn $ show x
    printer xs

filters :: [String] -> [String] -> [String]
filters xs [] = xs
filters xs (y:ys) = filters (filter (isInfixOf y) xs ) ys

stackGrep :: [String] -> [String] -> IO ()
stackGrep xs ys = do 
    putStrLn $ "Filtering:" ++ show ys
    paraFiltro <- getLine
    if(paraFiltro == "pop" && null ys)
        then return ()
        else if(paraFiltro == "pop" && not (null ys))
        then do
            printer $ filters xs $ init ys
            stackGrep xs $ init ys
    else do
        printer $ filters xs $ ys ++ [paraFiltro]
        stackGrep xs $ ys ++ [paraFiltro]

