module SK where
  
import SASL 
import Data.Tree 

import Debug.Trace

type SK = Term

ppp :: Term -> String
ppp (App x@(App _ _) y@(App _ _)) =   (ppp x)  ++ " @ " ++ "(" ++(ppp y) ++ ")"
ppp (App x y@(App _ _)) =  (ppp x) ++ " @ " ++ "(" ++(ppp y) ++ ")"
ppp (App x@(App _ _) y) =   (ppp x)  ++ " @ " ++ (ppp y)

ppp (App x y) = (ppp x) ++ " @ " ++(ppp y)
ppp x = show x

toDataTree :: SK -> Data.Tree.Tree [Char]
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
removevar v (Op "cond") | null v  = Builtin Cond
                    | otherwise = App K (Builtin Cond)                    
removevar v (Op "hd") | null v  = Builtin Hd
                    | otherwise = App K (Builtin Hd)                    
removevar v (Op "tl") | null v  = Builtin Tl
                    | otherwise = App K (Builtin Tl)                    
removevar v (Op ":") | null v  = Builtin Cons
                    | otherwise = App K (Builtin Cons)                    
removevar v (Op op) | null v && isUnary op = Builtin (Unnary op)
                    | isUnary op = App K (Builtin (Unnary op))
removevar v (Op op) | null v  = Builtin (Binary op)
                    | otherwise = App K (Builtin (Binary op))                    
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

reduce ((Builtin (Binary op)):(App _ x):(App _ y):rest) = (App I (binaryOp op (eval [x]) (eval [y]))):rest 
reduce ((Builtin (Unnary op)):(App _ x):rest) = (App I (unnaryOp op (eval [x]))):rest 

reduce _ = []
-- reduce (x:xs) = error $ "reduce not deffined for " ++ (show (x:xs))
-- reduce [] = []
unnaryOp :: HaskellOpName -> SK -> SK
unnaryOp op x = case x of 
                  (Bol x') -> Bol (fnBool x')
                  (Num x') -> Num (fnInt x')
                  x' -> error $ "opperator " ++ op ++ "is not defined for arguments " ++ (show x')
                where
                    fnBool = case lookup op boolOps of 
                                 (Just f) -> f
                                 Nothing -> error $ "fnBool undefined operator " ++ op                  
                    fnInt = case lookup op intOps of 
                                 (Just f) -> f
                                 Nothing -> error $ "fnInt undefined operator " ++ op                  
                    boolOps = [("not",\x -> not (x::Bool))]
                    intOps = [("neg",\x -> -(x::Int))]
                    
isUnary :: HaskellOpName -> Bool
isUnary x | elem x ["not","neg"] = True
isUnary _ = False                  

binaryOp :: HaskellOpName -> SK -> SK -> SK
binaryOp op x y = case (x,y) of
                   (Num x',Num y') | intIntOp -> Num $ fnIntInt x' y'
                   (Num x',Num y') | intBoolOp -> Bol $ fnIntBool x' y'
                   (Bol x',Bol y') -> Bol $ fnBool x' y'
                   (x',y') -> error $ "opperator " ++ op ++ "is not defined for arguments " ++ (show (x',y'))
                  where 
                        fnIntInt = case lookup op intIntOps of 
                                        (Just f) -> f
                                        Nothing -> error $ "fnIntInt undefined operator " ++ op
                        fnIntBool = case lookup op intBoolOps of 
                                        (Just f) -> f
                                        Nothing -> error $ "fnIntBool undefined operator " ++ op
                        fnBool = case lookup op boolOps of 
                                        (Just f) -> f
                                        Nothing -> error $ "fnBool undefined operator " ++ op
                        intBoolOps = [("=",\x y -> (x::Int) == (y::Int)),
                                      ("~=",\x y -> (x::Int) /= (y::Int)),
                                      ("<",\x y -> (x::Int) < (y::Int)),
                                      (">",\x y -> (x::Int) > (y::Int)),
                                      ("<=",\x y -> (x::Int) <= (y::Int)),
                                      (">=",\x y -> (x::Int) >= (y::Int))]
                        intIntOps = [("+",\x y -> (x::Int) + (y::Int)),
                                     ("-",\x y -> (x::Int) - (y::Int)),
                                     ("*",\x y -> (x::Int) * (y::Int)),
                                     ("/",\x y -> (x::Int) `div` (y::Int))]
                        boolOps = [("=",\x y -> (x::Bool) == (y::Bool)),
                                   ("~=",\x y -> (x::Bool) /= (y::Bool)),
                                   ("and",\x y -> (x::Bool) && (y::Bool)),
                                   ("or",\x y -> (x::Bool) || (y::Bool))]
                        intBoolOp = or (map (==op) (map fst intBoolOps))
                        intIntOp = or (map (==op) (map fst intIntOps))
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

