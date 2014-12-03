module Main where
import Grammar
import SASL
import SK
import ParserST
import System.Environment  

parse:: Parser a -> String -> [(a, Pstring)]
parse p xs = runStateT (readStateT (strip p) (0,0)) ((0,0),xs)

parse_result :: Parser a -> String -> Maybe a
parse_result p s = case parse p s of
                      [] -> Nothing
                      xs -> Just $ (fst.head) xs
                    
compute :: String -> Term
compute t = eval [r]  where  p = case parse expr t of
                                      [] -> error "Unable to parse"
                                      [xs] -> fst xs
                                      x -> error $ "Syntax error" ++ show x
                             r = removevar "" p
             
traceCompute :: String -> IO ()
traceCompute t = let p = case parse expr t of
                       [] -> error "Unable to parse"
                       [xs] -> fst xs
                       x -> error $ "Syntax error" ++ show x
                     r = removevar "" p
                 in do putStrLn $ "Going to parse" ++ t
                       putStrLn "parse result:"
                       putStrLn (ppp p)
                       putStrLn "after reduction:"
                       putStrLn (ppp r)
                       putStrLn "Result:"
                       putStrLn $ ppp $ eval [r]
                       
-- compute "f 0 where f a = if a = 1 then 0 else a + f (a + 1)"
-- compute "f 0 where f a = if a = 1 then 14 else f (a + 1)"
-- compute "sum 5 where sum x = if x = 0 then 0 else x + sum (x-1)"
-- runhaskell Main.hs "f 0 where f a = if a = 1 then 0 else a + f (a + 1)"  > out.hs 2>&1
-- compute "f 0 + g where f a = if a = 1 then 14 else f (a + 1)\n             g = 2"

prop_1 = case  compute "f 0 where f a = if a = 1 then 14 else f (a + 1)" of (Num 14) -> True;_ -> False
prop_11 = case compute "sum 3 where \n sum x = if x=0 then 0 else x + sum (x-1)" of (Num 6) -> True;_->False
prop_2 = case  compute "f 1 9 where f a b = b " of (Num 9) -> True;_ -> False
prop_3 = case compute "f 2 + g 2 where \n f x = 2 + x\n g x = 5 + x*y where y = 4" of (Num 17) -> True;_->False
prop_4 = case compute "f 2 + sum 3 where \n f x = 2 + x\n sum x = if x=0 then 0 else x + (sum (x-1))" of (Num 10) -> True;_->False
prop_5 = case compute "(f g) 1 where\n f x y=x y \n g x = x + 3" of (Num 4) -> True;_->False
prop_6 =case  compute "hd (tl (tl (map f (1:2:3:nil)))) where\n map g x = if x = nil then nil else g (hd x) : map g (tl x)\n f x = x * x "of (Num 9) -> True;_->False

main :: IO ()
main =  do args <- getArgs
           putStrLn "The arguments are:"  
           _ <- mapM traceCompute args
           putStrLn "Done"
