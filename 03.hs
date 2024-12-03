import Text.Regex.Posix
import Data.List (isPrefixOf)

-- Part 1
checksum :: String -> Int
checksum str = foldr (eval . convert) 0 matches
    where eval (x,y) acc = x*y + acc
          matches = (str =~ "mul\\(([0-9]*),([0-9]*)\\)") :: [[String]]
          convert (a:b:c:_) = (read b, read c)

-- Part 2
checksumToggle :: String -> Int
checksumToggle str = foldr eval 0 $ skip True (map convert matches) []
    where
        skip t (("do()",_,_):xs) acc = skip True xs acc 
        skip t (("don't()",_,_):xs) acc = skip False xs acc 
        skip False ((_,_,_):xs) acc = skip False xs acc
        skip True (a:xs) acc = skip True xs acc ++ [a]
        skip _ [] acc = acc
        eval (_, x,y) acc = x*y + acc
        matches = (str =~ "mul\\(([0-9]*),([0-9]*)\\)|do\\(\\)|don't\\(\\)") :: [[String]]
        convert (a:b:c:_) = (a, read b, read c)
    
main :: IO()
main = do
    contents <- readFile "03.txt"
    print $ checksum contents
    print $ checksumToggle contents
