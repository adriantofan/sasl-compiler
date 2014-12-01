module Interpretor where

import SASL
import Grammar


import Control.Monad
import Control.Applicative hiding (many)
import Text.Show.Functions
import Debug.Trace
newtype I a = I {runI::a} deriving (Show)
type M = I
instance Functor I where
  fmap = liftM
instance Applicative I where
  pure  = return
  (<*>) = ap
  
instance Monad I where
  return x = I x
  (I i) >>= f = f i
  
data Value = Wrong String
            |IntValue Int
            |BoolValue Bool
            |StringValue String
            |FuncValue (M Value -> M Value)
            |DefValue Name
            deriving (Show)

type Environment = [(Name,M Value)]
showval :: Value -> String
showval = show

lookup_env :: Name -> Environment -> Maybe (M Value)
lookup_env x xs = lookup x xs

interp :: Term -> Environment -> M Value

interp (Num x) _ = return $ IntValue x 
interp (Bol x) _ = return $ BoolValue x 
interp (Op x) _ = return $ FuncValue (operator_computation x)
interp (Var x) e = case lookup_env x e of
                    (Just v) -> case v of 
                                  _ -> v
                    Nothing -> error $ "variable "++(show x)++" not found in environment" ++ (show e)
interp (Lam x t) e = return $ FuncValue (\x' -> interp t ((x,x'):e) )
interp (App t u) e = interp t e >>= \f -> apply f (interp u e)

interp (Def n t) e = do body<-interp t e;return Def
interp _ _ = error $ "apply not completely implemented"

apply :: Value -> M Value -> M Value
apply (FuncValue f) x = f x
apply x _ = error $ "apply only works with function arguments -> " ++ show x  

operator_computation :: String -> (M Value -> M Value)
operator_computation op = \t -> 
                            t >>= \x -> operator op x

operator :: String -> (Value -> M Value)
operator op| op == "*" = \x ->  case x of
                      (IntValue _) -> f_int_int_int (*) x
                      _ -> error $op ++ " is defined only for Int"                                            
           | op == "/" = \x ->  case x of
                      (IntValue _) -> f_int_int_int div x
                      _ -> error $op ++ " is defined only for Int"                                            
           | op == "+" = \x ->  case x of
                      (IntValue _) -> f_int_int_int (+) x
                      _ -> error $op ++ " is defined only for Int"                                            
           | op == "-" = \x ->  case x of
                      (IntValue _) -> f_int_int_int (-) x
                      _ -> error $op ++ " is defined only for Int"                                            
           | op == "neg" = \x -> case x of
                      (IntValue _) -> f_int_int negate x
                      _ -> error $op ++ " is defined only for Int"                                            
           | op == "=" = \x -> case x of
                      (IntValue _) -> f_int_int_bool (==) x
                      (BoolValue _) -> f_bool_bool_bool (==) x
                      _ -> error $op ++ " is defined only for Int and Bool"                                            
           | op == "~=" = \x -> case x of
                      (IntValue _) -> f_int_int_bool (/=) x
                      (BoolValue _) -> f_bool_bool_bool (/=) x
                      _ -> error $op ++ " is defined only for Int and Bool"                                            
           | op == "<" = \x -> case x of
                      (IntValue _) -> f_int_int_bool (<) x
                      (BoolValue _) -> f_bool_bool_bool (<) x
                      _ -> error $op ++ " is defined only for Int and Bool"                                            
           | op == ">" = \x -> case x of
                      (IntValue _) -> f_int_int_bool (>) x
                      (BoolValue _) -> f_bool_bool_bool (>) x
                      _ -> error $op ++ " is defined only for Int and Bool"                                            
           | op == ">=" = \x -> case x of
                      (IntValue _) -> f_int_int_bool (>=) x
                      (BoolValue _) -> f_bool_bool_bool (>=) x
                      _ -> error $op ++ " is defined only for Int and Bool"                                            
           | op == "<=" = \x -> case x of
                      (IntValue _) -> f_int_int_bool (<=) x
                      (BoolValue _) -> f_bool_bool_bool (<=) x
                      _ -> error $op ++ " is defined only for Int and Bool"                                            
           | op == "not" = \x -> f_bool_bool not x
           | op == "and" = \x ->  case x of
                      (BoolValue _) -> f_bool_bool_bool (&&) x
                      _ -> error $op ++ " is defined only for Bool"                                            
           | op == "or" = \x ->  case x of
                      (BoolValue _) -> f_bool_bool_bool (||) x
                      _ -> error $op ++ " is defined only for Bool"                                       
           | otherwise = error $ "operator "++ op ++ " unknown"

f_bool_bool ::(Bool -> Bool) -> Value -> M Value
f_bool_bool f (BoolValue x) =  return $ BoolValue $ f x
f_bool_bool _ _ = undefined
 
f_int_int :: (Int -> Int) -> Value -> M Value
f_int_int f (IntValue x) =  return $ IntValue $ f x
f_int_int _ _ = undefined

f_bool_bool_bool :: (Bool -> Bool -> Bool) -> Value -> M Value          
f_bool_bool_bool f (BoolValue x') = return $ FuncValue (\z ->
                                            do y<-z
                                               case y of 
                                                  (BoolValue y') ->return (BoolValue ((f x') y'))
                                                  _ -> error "expecting it as second argument")
f_bool_bool_bool _ _ = undefined                                              

f_int_int_int :: (Int -> Int -> Int) -> Value -> M Value          
f_int_int_int f (IntValue x') = return $ FuncValue (\z ->
                                            do y<-z
                                               case y of 
                                                 (IntValue y') -> return (IntValue ((f x') y'))
                                                 _ -> error "expecting it as second argument")
f_int_int_int _ _ = undefined
                                              
f_int_int_bool :: (Monad m)=> (Int -> Int -> Bool) -> Value -> m Value          
f_int_int_bool f (IntValue x') = return $ FuncValue (\z ->
                                            do y<-z
                                               case y of 
                                                 (IntValue y') -> return (BoolValue ((f x') y'))
                                                 _ -> error "expecting it as second argument")
f_int_int_bool _ _ = undefined

eval:: String -> (M Value,Term)
eval xs = (interp t [],t)
           where t = case parse_result expr xs of
                       Just x -> x
                       Nothing -> error "Syntax error from parser"
                       
execute :: String -> (([Term],Term),M Value)
execute xs = ((g,y),value) where
                 value = interp y $ trace ("\nenv "++show env++"\n") env
                 (g,y) = case parse_result program xs of
                               Just x -> x
                               Nothing -> error "Syntax error from parser or empty result"
                 env = map (\(Def x y) -> (x,let mvalue = interp y [] in trace (show mvalue) mvalue )) g