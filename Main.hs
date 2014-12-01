module Main where
import Grammar
import SASL
import SK
import ParserST


parse:: Parser a -> String -> [(a, Pstring)]
parse p xs = runStateT (readStateT (strip p) (0,0)) ((0,0),xs)

parse_result :: Parser a -> String -> Maybe a
parse_result p s = case parse p s of
                      [] -> Nothing
                      xs -> Just $ (fst.head) xs
                    
compute :: String -> IO ()
compute t = let p = case parse expr t of
                      [] -> error "Unable to parse"
                      [xs] -> fst xs
                      r -> error $ "Syntax error" ++ show r
                r = removevar "" p
            in do putStrLn "parse result:"
                  putStrLn (show p)
                  putStrLn "after reduction:"
                  putStrLn (show r)
                  putStrLn "Result:"
                  putStrLn $ show $ eval [r]
-- compute "x 2 where x a = if x = 0 then 0 else a + x (a - 1)"