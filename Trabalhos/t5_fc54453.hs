

main :: IO ()
main = do
    (file:_) <- getArgs
    content <- readFile file
    linhas <- lines content



printLines