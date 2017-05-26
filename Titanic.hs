import Text.CSV -- cabal install csv
import Data.List

main = do
 content <- parseCSVFromFile "titanic.csv"
 let Right tabl = content
 let headers    = head tabl
 let values     = init (tail tabl) -- 895*12
 let properties = map (\v -> zip headers v) values
 let transform ((_,s):ps) = map (\(p,v) -> (s,p,v)) ps
 let graph      = concat (map transform properties)
 print (graph!!100)
