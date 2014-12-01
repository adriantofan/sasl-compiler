module Main where
import Control.Monad
import Data.Char

import ParserST
import Grammar

matexpr:: Parser Int
matexpr = term `chainl1` addop
term:: Parser Int
term = factor `chainr1` expop
factor:: Parser Int
factor = nat +++ braket (char '(') matexpr (char ')')
addop:: Parser (Int->Int->Int)
addop = ops [(char '+', (+)),((char '-'), (-))]
expop:: Parser (Int->Int->Int)
expop = ops [(char '^',(^))]

                   
data Term = App Term Term        -- application
          | Lam String Term      -- lamda abstraction
          | Let [(String,Term)] Term -- local definition
          | Var String           -- variable
          deriving (Show, Eq)
          
expr :: Parser Term
expr  = atom `chainl1` (do return(App))
atom :: Parser Term
atom  = lam +++ local +++ var +++ paren
lam :: Parser Term
lam   = do _ <- symbol "\\"
           x <- variable
           _ <- symbol "->" 
           e <- expr
           return (Lam x e)
           
local :: Parser Term           
local = do _ <- symbol "let"
           ds <- many1_offside defn
           _ <- symbol "in"
           e'<- expr
           return (Let ds e')

defn :: Parser (String,Term)                      
defn = do  x <- variable
           _ <- symbol "="
           e <- expr
           return (x,e)
           
var :: Parser Term                      
var = do v <- variable
         return (Var v)
         
paren :: Parser Term         
paren = braket (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let","in"]






