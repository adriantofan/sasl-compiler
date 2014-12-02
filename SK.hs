module SK where
  
import SASL 
import Data.Tree 

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

ppp :: Term -> String
ppp (App x@(App _ _) y@(App _ _)) =   (ppp x)  ++ " @ " ++ "(" ++(ppp y) ++ ")"
ppp (App x y@(App _ _)) =  (ppp x) ++ " @ " ++ "(" ++(ppp y) ++ ")"
ppp (App x@(App _ _) y) =   (ppp x)  ++ " @ " ++ (ppp y)

ppp (App x y) = (ppp x) ++ " @ " ++(ppp y)
ppp x = show x

toDataTree :: Term -> Data.Tree.Tree [Char]
toDataTree (App x y) = Node "App" [toDataTree y,toDataTree x]
toDataTree (Def n t) = Node ("Def " ++ show n) [toDataTree t]
toDataTree (Lam x t) = Node ("Lam(" ++show x++")") [toDataTree t]
toDataTree (Where xs e) = Node ("Where") [Node "Exp" [(toDataTree e)],Node "Defs" (defs xs)]
                                    where 
                                      defs [] = []
                                      defs  ((n,t):ys) = (toDataTree (Def n t)):defs ys

toDataTree term = Node (show term) []

pp:: Term -> IO ()  
pp = putStrLn.drawTree.toDataTree 



removevar:: String -> Term -> SK
-- removevar v x | traceShow ("removevar v=",v," ,exp=",ppp x) False = undefined
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
                    | otherwise = App K (Var x)
-- where e = f@a
removevar v (App f x) | null v = App (removevar v f) (removevar v x)
                      | otherwise = (App (App S (removevar v f)) (removevar v x))
-- everyting else
removevar v S |null v = S
              |otherwise = App K S
removevar v K |null v = K
              |otherwise =  App K K
removevar v I |null v = I
              |otherwise = App K I
removevar v Y |null v = Y
              |otherwise = App K Y
removevar v (Builtin x)|null v = Builtin x
                       |otherwise =App K (Builtin x)
removevar _ Empty = Empty
removevar _ Nil = Nil
-- everyting else !!?!?!?
removevar v x = error $ "removevar not defined for variable " ++ (show v) ++ " and term "  ++ (show x)

eval :: [SK] -> SK
-- eval xs | traceShow ("eval",xs) False = undefined
eval [] = Empty
eval x@((App p _):_) = eval $ p:x
eval [x] = x
eval xs = if null reduced  then (head xs) else eval reduced
          where reduced = reduce xs
 
-- reduce (Num x) = Num x
reduce :: [SK] -> [SK]
-- reduce xs | traceShow ("reduce",map ppp (take 3 xs)) False = undefined
reduce (I:(App _ x):xs) = x:xs
reduce (K:(App _ x):(App _ _):rest) = (App I x):rest              -- K combinator
reduce (S:(App _ f):(App _ g):(App _ x):rest) =  (App (App f x) (App g x)):rest   -- S combinator                     
reduce (Y: recurse@(App _ f):rest) = (App f (App Y f)):rest -- Y combinator

reduce (Nil:rest) = Nil:rest
reduce ((Builtin Cond):(App _ c):(App _ x):(App _ y):rest) = case eval [c] of
                                                                (Bol True) -> (App I x):rest 
                                                                (Bol False) -> (App I y):rest
                                                                r -> error $ "cond is deffined only for Bool. Got " ++ (show r)
reduce ((Builtin Hd):(App _ x):rest) = case eval [x] of
                                          (Pair y _) -> y:rest
                                          Nil -> Nil:[]
                                          r -> error $ "head should be only called on a list. Got " ++ (show r)
reduce ((Builtin Tl):(App _ x):rest) = case eval [x] of
                                          (Pair _ y) -> y:rest
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

isRecusive :: String->Term->Bool
-- isRecusive n x | traceShow ("isRevursive",n,x) False = undefined
isRecusive n (Var x) =  x == n
isRecusive n (App x y) = isRecusive n x || isRecusive n y
isRecusive n (Bol _) = False
isRecusive n (Num _) = False
isRecusive n (Builtin _) = False
isRecusive n (Lam x _) = x /= n -- is not recursive if the lambda captures the name
isRecusive n (Def x _) = x /= n -- is not recursive if the lambda captures the name
isRecusive n (Where n_t_pairs e) = or (map rec n_t_pairs) || isRecusive n e
                                   where rec = \(n',t') -> not (n' == n) || isRecusive n t' -- no recurse
isRecusive n (Pair x y) = isRecusive n x || isRecusive n y                                  
isRecusive n (Op _) = False
isRecusive n x | traceShow ("isRevursive not implemented for ",n,x) False = undefined
isRecusive _ _ = False

-- takes a Where definition and transforms it in function application by abstracting variables
-- 3 + f == 5  where f = 1 + 1  
-- where f = (App (App (Op "+") (Num 1)) (Num 1)) in  (App (App (Op "+") (Num 3)) (Var "f"))
-- Where [("f",(App (App (Op "+") (Num 1)) (Num 1)))] (App (App (Op "+") (Num 3)) (Var "f"))
-- >removevar "" $ Where [("f",(App (App (Op "+") (Num 1)) (Num 1)))] (App (App (Op "+") (Num 3)) (Var "f"))
-- >App (App (App S (App (App S (App K (Builtin Plus))) (App K (Num 3)))) I) (App (App (Builtin Plus) (Num 1)) (Num 1))

transform_where2app:: Term -> Term
-- transform_where2app (Where ([(n,e)]) x) | traceShow ("transform_where2app",n,e,x) False = undefined
transform_where2app (Where ([(n,e)]) x) = case e of 
                                               (Lam x' e') | isRecusive n e' -> App (removevar n x) (App Y (removevar n (removevar x' e')))
                                               (Lam x' e') | otherwise -> App (removevar n x) (removevar x' e')    
                                               _   -> App (removevar n x) e
                                               
transform_where2app x = error $ "transform_where2app is not defined for" ++ (show x)

