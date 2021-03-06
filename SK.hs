module SK where
import Prelude hiding ((!!))  
import SASL 

import Debug.Trace

type SK = Term

infixl 9 !! -- first abstract
infixl 8 @@ -- then App

(@@):: Term->Term->Term
(@@) = App

(!!):: String -> Term -> SK
(!!) = abstract

-- prety prints a term
pTerm :: Term -> String
pTerm (App x@(App _ _) y@(App _ _)) =   (pTerm x)  ++ " @ " ++ "(" ++(pTerm y) ++ ")"
pTerm (App x y@(App _ _)) =  (pTerm x) ++ " @ " ++ "(" ++(pTerm y) ++ ")"
pTerm (App x@(App _ _) y) =   (pTerm x)  ++ " @ " ++ (pTerm y)
pTerm (App x y) = (pTerm x) ++ " @ " ++(pTerm y)
pTerm (Pair x y) = "("++(pTerm x)++")" ++ ":" ++"("++(pTerm y)++")"
pTerm x = show x

abstract:: String -> Term -> SK
-- abstract v x | traceShow ("abstract v=",v," ,exp=",pTerm x) False = undefined
abstract v d@(Where _ _) = abstract v (compileWhere d)
abstract v (Lam v' t) = abstract v (abstract v' t)

-- where e = constant c
abstract v (Num x)| null v  = Num x 
                   | otherwise = App K (Num x)
abstract v (Bol x)| null v  = Bol x 
                   | otherwise = App K (Bol x)                   
-- where e = var(v)                   
abstract v (Op "cond") | null v  = Builtin Cond
                    | otherwise = App K (Builtin Cond)                    
abstract v (Op "hd") | null v  = Builtin Hd
                    | otherwise = App K (Builtin Hd)                    
abstract v (Op "tl") | null v  = Builtin Tl
                    | otherwise = App K (Builtin Tl)                    
abstract v (Op ":") | null v  = Builtin Cons
                    | otherwise = App K (Builtin Cons)                    
abstract v (Op op) | null v && isUnary op = Builtin (Unnary op)
                    | isUnary op = App K (Builtin (Unnary op))
abstract v (Op op) | null v  = Builtin (Binary op)
                    | otherwise = App K (Builtin (Binary op))                    
abstract v (Var x) | null v  = Var x
                    | v == x = I
                    | otherwise = App K (Var x)
-- where e = f@a
abstract v (App f x) | null v = App (abstract v f) (abstract v x)
                      | otherwise = (App (App S (abstract v f)) (abstract v x))
-- everyting else
abstract v S |null v = S
              |otherwise = App K S
abstract v U |null v = U
              |otherwise = App K U
abstract v K |null v = K
              |otherwise =  App K K
abstract v I |null v = I
              |otherwise = App K I
abstract v Y |null v = Y
              |otherwise = App K Y
abstract v (Builtin x)|null v = Builtin x
                       |otherwise =App K (Builtin x)
abstract _ Empty = Empty
abstract v Nil |null v = Nil
                |otherwise = App K Nil
-- everyting else !!?!?!?
abstract v x = error $ "abstract not defined for variable " ++ (show v) ++ " and term "  ++ (show x)

eval :: [SK] -> SK
-- eval xs | traceShow ("eval",xs) False = undefined
eval [] = Empty
eval x@((App p _):_) = eval $ p:x
eval [x] = x
eval xs = if null reduced  then (head xs) else eval reduced
          where reduced = reduce xs
 
reduce :: [SK] -> [SK]
-- reduce xs | traceShow ("reduce",map pTerm (take 3 xs)) False = undefined
reduce (I:(App _ x):xs) = x:xs
reduce (K:(App _ x):(App _ _):rest) = (App I x):rest              -- K combinator
reduce (S:(App _ f):(App _ g):(App _ x):rest) =  (App (App f x) (App g x)):rest   -- S combinator                     
reduce (Y:(App _ f):rest) = (App f (App Y f)):rest -- Y combinator
reduce (U:(App _ f):(App _ z):rest) = (f @@ ((Builtin Hd) @@ z) @@ ((Builtin Tl) @@ z)):rest
reduce (Nil:rest) = Nil:rest
reduce ((Builtin Cond):(App _ c):(App _ x):(App _ y):rest) = case eval [c] of
                                                                (Bol True) -> (App I x):rest 
                                                                (Bol False) -> (App I y):rest
                                                                r -> error $ "cond is deffined only for Bool. Got " ++ (show r)
reduce ((Builtin Hd):(App _ x):rest) = case eval [x] of
                                          (Pair y _) -> y:rest
                                          r -> error $ "head should be only called on a list. Got " ++ (show r)
reduce ((Builtin Tl):(App _ x):rest) = case eval [x] of
                                          (Pair _ y) -> y:rest
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
                    boolOps = [("not",\x' -> not (x'::Bool))]
                    intOps = [("neg",\x' -> -(x'::Int))]
                    
isUnary :: HaskellOpName -> Bool
isUnary x | elem x ["not","neg"] = True
isUnary _ = False                  

binaryOp :: HaskellOpName -> SK -> SK -> SK
binaryOp op a b = case (a,b) of
                   (Nil,Nil) -> Bol True
                   (Nil,_) -> Bol False
                   (_,Nil) -> Bol False
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

isRecursive :: String->Term->Bool
-- isRecursive n x | traceShow ("isRevursive",n,x) False = undefined
isRecursive n (Var x) =  x == n
isRecursive n (App x y) = isRecursive n x || isRecursive n y
isRecursive _ (Bol _) = False
isRecursive _ (Num _) = False
isRecursive _ (Builtin _) = False
isRecursive n (Lam x y) = x /= n && isRecursive n y-- is not recursive if the lambda captures the name
isRecursive n (Def x y) = x /= n && isRecursive n y-- is not recursive if the lambda captures the name
isRecursive n (Where n_t_pairs e) = or (map rec n_t_pairs) || isRecursive n e
                                  where rec = \(n',t') -> not (n' == n) && isRecursive n t' -- no recurse
isRecursive n (Pair x y) = isRecursive n x || isRecursive n y                                  
isRecursive _ (Op _) = False
isRecursive _ Nil = False
isRecursive n x | traceShow ("isRevursive not implemented for ",n,x) False = undefined
isRecursive _ _ = False

-- compiles a where expresion in a variable free lambda
compileWhere:: Term -> Term
-- compileWhere x | traceShow ("compileWhere",x) False = undefined
compileWhere (Where ([(n,e)]) x) = case e of 
   (Lam x' e') | isRecursive n e' -> App (abstract n x) (App Y (abstract n (abstract x' e')))
   (Lam x' e') | otherwise -> App (abstract n x) (abstract x' e')    
   _   -> App (abstract n x) e
compileWhere (Where ([(f,e2),(g,e3)]) e1) = case rec of
      True -> U @@ ( f!!(U @@ g!!(K @@ e1))) @@ (Y @@ (U @@ (f!!(U @@ g!!(K @@ (toList [""!!e2,""!!e3]))))))
      False -> U @@ (f!!(U @@ g!!(K @@ e1))) @@ (toList [""!!e2,""!!e3])
  where rec = (isRecursive g e2) || (isRecursive f e3) || (isRecursive f e2) || (isRecursive g e3) 
compileWhere x = error $ "compileWhere is for more than two definitions in" ++ (show x)

toList :: [Term]->Term
toList xs = foldr (\x y -> (Builtin Cons) @@ x @@ y) Nil xs