import Text.Regex.Posix

checksum :: String -> Int
checksum str = foldr (eval . convert) 0 matches
    where eval (x,y) acc = x*y + acc
          matches = (str =~ "mul\\(([0-9]*),([0-9]*)\\)") :: [[String]]
          convert (a:b:c:_) = (read b, read c)

main :: IO()
main = do
    contents <- readFile "03.txt"
    print $ checksum contents
