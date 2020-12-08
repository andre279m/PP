import System.Environment

toFile :: Show a => FilePath -> [a] -> IO()
toFile file xs = writeFile file $ unlines $ map show xs

fromFile :: Read a => FilePath -> IO [a]
fromFile file = do
    contents <- readFile file
    return $ map read $ lines contents

filterFiles :: (String -> Bool) -> FilePath -> FilePath -> IO()
filterFiles f file1 file2 = do
    contents <- fromFile file1
    toFile file2 $ filter f contents

filterPrefix :: String -> FilePath -> FilePath -> IO()
filterPrefix = filterFiles . isPrefix

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = False
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x==y && isPrefix xs ys

main :: IO()
main = do
    (x:y:z:_) <- getArgs
    filterPrefix x y z
