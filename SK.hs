module SK where
  
import SASL
import Data.Tree 

import Control.Monad
import Control.Applicative hiding (many)
import Text.Show.Functions
import Debug.Trace


type SK = Term            
-- data SK =  S
--           |K
--           |I
--           |Builtin Func
--           |Num Int
--           |App SK SK
--           |Var String
--           |Empty
--           deriving (Show)
-- data Term = Str [Char]
--             |Bol Bool
--             |Num Int
--             |Var Name
--             |App Term Term        -- application (lamda) value
--             |Lam Name Term
--             |Op String            -- opperator
--             |Def Name Term
--             |Where [(Name,Term)] Term    -- Where e1,e2,e3 in  e
--             |S
--             |K
--             |I
--             |Builtin Func
--             |Pair Term Term
--             |Nil
--             |Empty
--             deriving (Show)
-- data Func = Plus
--             |Cons
--             |Hd
--             |Tl
--             |Cond
toDataTree :: Term -> Data.Tree.Tree [Char]
toDataTree (App x y) = Node "App" [toDataTree y,toDataTree x]
toDataTree (Def name t) = Node ("Def " ++show name) [toDataTree t]
toDataTree (Lam x t) = Node ("Lam(" ++show x++")") [toDataTree t]
toDataTree (Where xs e) = Node ("Where") [Node "Exp" [(toDataTree e)],Node "Defs" (defs xs)]
                                    where 
                                      defs [] = []
                                      defs  ((n,t):xs) = (toDataTree (Def n t)):defs xs

toDataTree term = Node (show term) []

pp:: Term -> IO ()  
pp = putStrLn.drawTree.toDataTree 



removevar:: String -> Term -> SK
removevar v d@(Where _ _) = removevar v (transform_where2app d)
removevar v (Lam v' t) = removevar v (removevar v' t)

-- where e = constant c
removevar v (Num x)| null v  = Num x 
                   | otherwise = App K (Num x)
removevar v (Bol x)| null v  = Bol x 
                   | otherwise = App K (Bol x)                   
-- where e = var(v)                   
removevar v (Op "+") | null v  = Builtin Plus
                    | otherwise = App K (Builtin Plus)
removevar v (Op "=") | null v  = Builtin Comp
                    | otherwise = App K (Builtin Comp)
removevar v (Op "cond") | null v  = Builtin Cond
                    | otherwise = App K (Builtin Cond)                    
removevar v (Op "hd") | null v  = Builtin Hd
                    | otherwise = App K (Builtin Hd)                    
removevar v (Op "tl") | null v  = Builtin Tl
                    | otherwise = App K (Builtin Tl)                    
removevar v (Op ":") | null v  = Builtin Cons
                    | otherwise = App K (Builtin Cons)                    
removevar v (Var x) | null v  = Var x
                    | v == x = I
                    | otherwise = App K (Var v)
-- where e = f@a
removevar v (App f x) | null v = App (removevar v f) (removevar v x)
                      | otherwise = (App (App S (removevar v f)) (removevar v x))
-- everyting else
removevar _ S = S
removevar _ K = K
removevar _ I = I
removevar _ (Builtin x) = Builtin x
removevar _ Empty = Empty
removevar _ Nil = Nil
-- everyting else !!?!?!?
removevar v x = error $ "removevar not defined for variable " ++ (show v) ++ " and term "  ++ (show x)

eval :: [SK] -> SK
-- eval xs | traceShow xs False = undefined
eval [] = Empty
eval x@((App p _):_) = eval $ p:x
eval [x] = x
eval xs = if null reduced  then (head xs) else eval reduced
          where reduced = reduce xs
 
-- reduce (Num x) = Num x
reduce :: [SK] -> [SK]
-- reduce xs | traceShow xs False = undefined
reduce (I:(App _ x):(App _ y):rest) = (App x y):rest              -- I combinator
reduce (I:(App _ x):[]) = x:[]
reduce (K:(App _ x):(App _ y):rest) = (App I x):rest              -- K combinator
reduce (S:(App _ f):(App _ g):(App _ x):rest) =  l:a:rest where   -- S combinator
                                                   a = (App l r)  
                                                   l = (App f x) 
                                                   r = (App g x) 
reduce (Y: recurse@(App _ f):rest) =(App f recurse):rest -- Y combinator

reduce (Nil:rest) = Nil:rest
reduce ((Builtin Cond):(App _ c):(App _ x):(App _ y):rest) = case eval [c] of
                                                                (Bol True) -> (App I x):rest 
                                                                (Bol False) -> (App I y):rest
                                                                r -> error $ "cond is deffined only for Bool. Got " ++ (show r)
reduce ((Builtin Hd):(App _ x):rest) = case eval [x] of
                                          p@(Pair x _) -> x:rest
                                          Nil -> Nil:[]
                                          r -> error $ "head should be only called on a list. Got " ++ (show r)
reduce ((Builtin Tl):(App _ x):rest) = case eval [x] of
                                          p@(Pair _ y) -> y:rest
                                          Nil -> Nil:[]
                                          r -> error $ "tail should be only called on a list. Got " ++ (show r) ++ "instead"      
reduce ((Builtin Cons):(App _ x):(App _ y):rest) = (App I (Pair x y)):rest
reduce ((Builtin Plus):(App _ x):(App _ y):rest) = (App I ((eval [x]) `skplus` (eval [y]))):rest 
reduce ((Builtin Comp):(App _ x):(App _ y):rest) = (App I ((eval [x]) `skcomp` (eval [y]))):rest 

reduce _ = []
-- reduce (x:xs) = error $ "reduce not deffined for " ++ (show (x:xs))
-- reduce [] = []

skcomp :: SK -> SK -> SK
(Num x) `skcomp` (Num y) = Bol (x == y)
p `skcomp` q = error $ "skcomp is defined only for integers.\n" ++ (show p) ++ "\n" ++ (show q)


skplus :: SK -> SK -> SK
(Num x) `skplus` (Num y) = Num (x + y)
p `skplus` q = error $ "skplus is defined only for integers.\n" ++ (show p) ++ "\n" ++ (show q)


notTree:: SK -> Bool
notTree (App _ _) = False
notTree _ = True

isRecursive::String->Term->Bool
isRecusive n x | n == "ss" = True
isRecursive n x = False
-- takes a Where definition and transforms it in function application by abstracting variables
-- 3 + f == 5  where f = 1 + 1  
-- where f = (App (App (Op "+") (Num 1)) (Num 1)) in  (App (App (Op "+") (Num 3)) (Var "f"))
-- Where [("f",(App (App (Op "+") (Num 1)) (Num 1)))] (App (App (Op "+") (Num 3)) (Var "f"))
-- >removevar "" $ Where [("f",(App (App (Op "+") (Num 1)) (Num 1)))] (App (App (Op "+") (Num 3)) (Var "f"))
-- >App (App (App S (App (App S (App K (Builtin Plus))) (App K (Num 3)))) I) (App (App (Builtin Plus) (Num 1)) (Num 1))
transform_where2app:: Term -> Term
transform_where2app (Where ([(name,e)]) x) = case e of 
                                               (Lam x' e') | isRecursive name e -> App (removevar name x) (App Y (removevar name (removevar x' e')))
                                               (Lam x' e') | otherwise -> App (removevar name x) (removevar x' e')    
                                               -- (Lam x' e') -> App (removevar name x) (removevar x' e')
                                               otherwise   -> App (removevar name x) e
                                               
transform_where2app x = error $ "transform_where2app is not defined for" ++ (show x)

