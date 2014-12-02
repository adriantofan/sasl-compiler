module Grammar where 
import ParserST
import Control.Monad
import Data.Char
-- import Debug.Trace
-- import Data.Maybe

infixr 5 +++

  -- map           :: (a -> b) -> (Parser a -> Parser b)
  -- result        :: a -> Parser a
  -- bind          :: Parser a -> (a -> Parser b) -> Parser b
  -- zero          :: Parser a
  -- (++)          :: Parser a -> Parser a -> Parser a
  -- update        :: (Pstring -> Pstring) -> Parser Pstring
  -- set           :: Pstring -> Parser Pstring
  -- fetch         :: Parser Pstring
fetch_pos::Parser Pstring
fetch_pos = fetch

env_pos::Parser Pos
env_pos = env
 
  -- env           :: Parser Pos
  -- setenv        :: Pos -> Parser a -> Parser a

--- Primitive parser combinators -------------------------------------
  
item :: Parser Char
item = do (pos,xs) <- update newstate
          defpos <-  env
          case xs of
            -- x:_ | onside (trace ("pos"++show pos) pos) (trace ("defpos"++show defpos) defpos) -> return x
            x:_ | onside pos defpos -> return x
            _ -> mzero
          
--
onside :: Pos -> Pos -> Bool
onside (l,c) (dl,dc) = (c > dc) || (l == dl)

newstate :: Pstring -> Pstring
newstate ((l,c),[]) = ((l,c),[])
newstate ((l,c),x:xs) = (newpos,xs)
                         where
                           newpos = case x of
                                     '\n' -> (l+1,0)
                                     '\t' -> (l,((c `div` 8)+1)*8)
                                     _ -> (l,c+1)
--- Derived combinators ----------------------------------------------

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
p `chainl1` op = p >>= rest
                      where rest x = (do f <- op
                                         y <- p
                                         rest (f x y)) +++ (return x)

chainr1  :: Parser a -> Parser (a->a->a) -> Parser a
p `chainr1` op = p >>= \x ->
                 (do f <- op
                     y <- p `chainr1` op
                     return (f x y)) +++ return x

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

class Monad m => MonadPlusPlus m where
  (+++):: m a -> m a -> m a

instance MonadPlusPlus [] where    
  [] +++ b = b
  a +++ _ = a
instance MonadPlusPlus m => MonadPlusPlus(StateT m s)  where    
  p +++ q = StateT (\s -> (runStateT p) s +++ (runStateT q) s )
instance MonadPlusPlus m => MonadPlusPlus(ReaderT m s)  where    
  p +++ q = ReaderT (\s -> (readStateT p) s +++ (readStateT q) s )
  
sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else mzero

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = p `sepby1` sep +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep  = do x <- p
                     xs <- many (do _ <- sep
                                    p)
                     return (x:xs)
                     
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket left t right = do _ <- left
                          x <- t
                          _ <- right
                          return (x)
                         
ops :: [(Parser a, b)] -> Parser b
ops os = foldr1 (+++) [(do _<-p;return f) |(p,f)<- os]

--- Usefull parsers ----------------

char :: Char -> Parser Char
char c = sat (==c)

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isLetter

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)
         
int :: Parser Int
int = (do _ <- char '-'
          n <- nat
          return (negate n)) `mplus` nat
          
string :: String -> Parser String
string [] = return []
string (x:xs) = do _  <- char x
                   _ <- string xs
                   return (x:xs)

ident :: Parser String
ident = do  x  <- letter +++ char '_'
            xs <- many (alphanum +++ char '_')
            return (x:xs)

--- Lexical combinators ----------------------------------------------

spaces :: Parser ()
spaces = do _ <- many1 $ sat isSpace'
            return ()
         where 
           isSpace' x = (x == ' ') || (x == '\n' || (x =='\t')) 
           
comment :: Parser ()
comment = do _ <- string "--"
             _ <- many (sat (\x -> x /= '\n'))
             return ()
             
junck :: Parser ()
junck = do _ <- setenv (0::Int,-1::Int) (many (spaces +++ comment))
           return ()
           
-- parse already invokes junk befor p           
-- prop_junck = 'n' == fromJust (parse_result (do {junck;item}) "   \n   \tn")

strip         :: Parser a -> Parser a
strip p = do _ <- junck
             p

token :: Parser a -> Parser a
token p = do v <- p 
             _ <- junck
             return (v)

-- prop_token = "11" == fromJust (parse_result (token (many digit)) "11   ") -- parses and removes trailing junk(whitespace)
             
many1_offside  :: Parser a -> Parser [a]
many1_offside p = do (pos,_) <- fetch_pos
                     setenv pos (many1 (off p))

many_offside  :: Parser a -> Parser [a]
many_offside p = many1_offside p +++ return [] 


off :: Parser a -> Parser a
off p = do (_,dc) <- env_pos
           ((l,c),_) <- fetch_pos
           guard (c == dc)
           setenv (l,c) p
--   ↓ setenv to save the current position for the whole group (accesible with env_pos)  
--   def1 <- if curent positions c = env's c, setenv (l,c) for the curent p (def1)
--   def2 <- if curent positions c = env's c, setenv (l,c) for the curent p (def1)
--   def3 <- if curent positions c = env's c, setenv (l,c) for the curent p (def1)
--  something else c nolonger equals env's c


--- Token parsers ----------------------------------------------------

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ks = token (do x <- ident
                          if not (elem x ks) then return (x) else mzero)
                          