module Main where
import Control.Monad
import Data.Char

import ParserST
import Grammar

-- data List a = Nil | Cons a (List a)
-- data Tree a b = Leaf a
--                 | Node (Tree a b, b, Tree a b)

type Data = (String,           -- type name
            [String],          -- parameters
            [(String,[Type])]) -- constructors and arguments
            
data Type = Arrow Type Type     -- function
           | Apply Type Type    -- application
           | Var String         -- variable
           | Con String         -- constructor
           | Tuple [Type]       -- tuple
           | List Type          -- list
           deriving (Show, Eq)
           

datadecls :: Parser [Data]
datadecls = many_offside datadecl

datadecl :: Parser Data
datadecl = do _ <- symbol "data"
              x <- constructor
              xs <- many variable
              _ <- symbol "="
              b <- condecl `sepby1` symbol "|" 
              return (x,xs,b)

constructor :: Parser String
constructor = token $ do  x  <- upper
                          xs <- many alphanum
                          return (x:xs)

variable :: Parser String
variable = identifier ["data"]

condecl :: Parser (String,[Type])
condecl = do x  <- constructor
             ts <- many type2
             return (x,ts)             
type0       = type1 `chainr1` (do _ <- symbol "->"; return Arrow)
type1       = type2 `chainl1` (return Apply)

type2 :: Parser Type
type2 = var +++ con +++ list +++ tuple

var :: Parser Type
var = do x <- variable ; return $ Var x

con :: Parser Type
con = do x <- constructor; return $ Con x

list :: Parser Type
list = do x <- bracket (symbol "[") type0 (symbol "]")
          return $ List x

tuple :: Parser Type
tuple = do ts <- bracket (symbol "(") (type0 `sepby` symbol ",") (symbol ")")
           return $ f ts
        where f [t] = t
              f ts = Tuple ts
