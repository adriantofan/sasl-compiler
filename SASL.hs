module SASL where
  
import Control.Monad
import Data.Char
-- import Text.Show.Functions

import ParserST
import Grammar

type Name = String
type HaskellOpName = String
data Func =  Unnary HaskellOpName
            |Binary HaskellOpName
            |Cond
            |Hd
            |Tl
            |Cons
            deriving (Show,Eq)
            
  
data Term = Str [Char]
            |Bol Bool
            |Num Int
            |Var Name
            |App Term Term        -- application (lamda) value
            |Lam Name Term
            |Op String            -- opperator
            |Def Name Term
            |Where [(Name,Term)] Term    -- Where e1,e2,e3 in  e
            |S
            |K
            |I
            |Y
            |U
            |Builtin Func
            |Pair Term Term
            |Nil
            |Empty
            deriving (Show)

program :: Parser ([Term],Term)
program = (do xs <- many_offside global_def
              _ <- symbol "."
              p <- expr
              return (xs,p))
            +++ (do p <- expr
                    return ([],p))
           
global_def :: Parser Term 
global_def =  do _ <- symbol "def"
                 n <- name
                 a <- abstraction
                 return $ Def n a

abstraction :: Parser Term
abstraction = do _ <- symbol "="
                 -- x <- expr
                 -- return $ Lam "" x
                 expr
              +++ do n <- name
                     a <- abstraction
                     return $ Lam n a
def :: Parser Term 
def = do n <- name
         a <- abstraction
         return $ Def n a
            
expr :: Parser Term
expr = do e <- condexpr
          (do d <- exprs''
              return (makeWhere' d e)) +++ (return e)
              
exprs'' :: Parser [Term]
exprs'' = do _ <- symbol "where"
             many1_offside def
           
makeWhere':: [Term] -> Term -> Term 
makeWhere' defs a = Where (map defToPair defs) a
  where defToPair x = case x of
                        (Def n val)-> (n,val)
                        _ -> error $ "where definition  should contain only defs. got "++ "" ++" instead"
              
-- makeWhere:: Term -> Term -> Term
-- makeWhere (Def n val) a = Where [(n,val)] a
-- makeWhere x y = error $ "makeWhere not defined for" ++ show x ++ show y
--
-- expr' :: Parser Term
-- expr' = do _ <- symbol "where"
--            def
                
        -- "where" (many def) expr'
        -- +++ ""

condexpr :: Parser Term
condexpr = do _ <- symbol "if"
              c <- expr
              _ <- symbol "then"
              x <- condexpr 
              _ <- symbol "else"
              y <- condexpr
              return (App (App (App (Op "cond") c) x) y)
           +++ listexpr


listexpr :: Parser Term
listexpr = opexpr `chainr1` ops [(symbol  ":" , infix_op ":")]

opexpr :: Parser Term
opexpr = conjunct `chainl1` ops [(symbol  "or" , infix_op "or")]  

conjunct :: Parser Term
conjunct = compar `chainl1` ops [(symbol  "and" , infix_op "and")]

compar :: Parser Term
compar = add `chainl1` compop

add :: Parser Term
add = mul `chainl1` addop

mul:: Parser Term   
mul = factor `chainl1` mulop   

factor:: Parser Term
factor = (do p<-prefix
             c<-comb
             case p of
                 "+" -> return c
                 "-" -> return (App (Op "neg") c)
                 "not" -> return (App (Op "not") c)
                 _ -> error "missing prefix operator") +++ comb
         
comb:: Parser Term
comb  = simple `chainl1` (do return(App))            

simple :: Parser Term
simple = variable +++ builtin +++ constant +++ paren

builtin :: Parser Term
builtin = do _ <- symbol "hd"
             return (Op "hd") 
          +++ do _ <- symbol "tl"
                 return (Op "tl")

paren :: Parser Term         
paren = bracket (symbol "(") expr (symbol ")") -- put exp here

constant :: Parser Term         
constant = num +++ bool +++ str +++ nil -- +++ list

variable :: Parser Term
variable = do v <- name 
              return (Var v)

bool :: Parser Term
bool = do x <- symbol "true" +++ symbol "false"
          case x of
           "true" -> return $ Bol True
           "false" -> return $ Bol False
           _ -> mzero

num :: Parser Term
num = do x <- natural
         return (Num x)

nil :: Parser Term
nil = do _ <- (symbol "nil")
         return Nil
        
str :: Parser Term
str = do x <- bracket (char '\"') (many escaped_ascii) (char '\"')
         return $ Str x

compop:: Parser (Term -> Term -> Term)
compop = ops [(symbol  "=" , infix_op "="),
              ((symbol "~="), infix_op "~="),
              ((symbol "<" ), infix_op "<"),
              ((symbol ">" ), infix_op ">"),
              ((symbol ">="), infix_op ">="),
              ((symbol "<="), infix_op "<=")]         

          
addop :: Parser (Term -> Term -> Term)          
addop = ops [(symbol "+", infix_op "+"),((symbol "-"), infix_op "-")]          
          
mulop :: Parser (Term -> Term -> Term)          
mulop = ops [(symbol "*", infix_op "*"),((symbol "/"), infix_op "/")]

--- More Usefull parsers ----------------

name :: Parser String
name = identifier ["hd","tl","true","false","not","and","or","def","nil","then","else","where"] -- shoul we add reserved words?

prefix :: Parser String
prefix = symbol "+" +++ symbol "-" +++ symbol "not"
    
infix_op :: String->Term->Term->Term
infix_op s = \a b -> (App (App (Op s) a) b) 

ascii :: Parser Char
ascii = sat isAscii 
        
escaped_ascii :: Parser Char
escaped_ascii =  (do {_ <- string "\\\"";return '"'}) +++ 
                 (do{ x<-ascii;guard(x/='"');return x})
                 
