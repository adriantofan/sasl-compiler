{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ParserST where
import Control.Monad
import Control.Applicative hiding (many)
    
class (Monad m) => MonadState m s where
  update :: (s->s) -> m s
  set :: s -> m s
  fetch :: m s
  set s = update (\_ -> s)
  fetch = update id
    
instance Monad m => Functor (StateT m s) where
    fmap = liftM   
                                 
instance Monad m => Applicative (StateT m s) where
    pure  = return
    (<*>) = ap
    
instance MonadPlus m => Alternative (StateT m s) where
    (<|>) = mplus
    empty = mzero
    
newtype StateT m s a = StateT {runStateT ::(s -> m (a,s))}

instance (Monad m) => Monad (StateT m s) where
  -- return :: a -> StateT m s a
  return a = StateT $ \s -> return (a,s)
  -- >>= StateT m s a -> (a -> StateT m s b) -> StateT m s b
  (StateT x) >>= f = StateT $ \s -> do (v,s') <- x s
                                       runStateT (f v) s'                                
                                       
instance MonadPlus m => MonadPlus (StateT m s) where
  mzero   = StateT $ \_ -> mzero
  (StateT p) `mplus` (StateT q) = StateT $ \s -> p s `mplus` q s
  
instance (Monad m) => MonadState (StateT m s) s where
  update f = StateT $ \s -> return (s,f s) 

newtype ReaderT m s a = ReaderT {readStateT::(s -> m a)}

instance (Monad m) =>Functor (ReaderT m s) where
    fmap = liftM   
                                 
instance (Monad m) => Applicative (ReaderT m s) where
    pure  = return
    (<*>) = ap
instance MonadPlus m => Alternative (ReaderT m s) where
    (<|>) = mplus
    empty = mzero
class Monad m => ReaderMonad m s where
  env :: m s
  setenv :: s -> m a -> m a
      
instance (Monad m) => Monad (ReaderT m s) where
  -- return :: a -> ReaderT
  -- >>= ReaderT a -> (a -> ReaderT b) -> ReaderT b
  return v = ReaderT $ \_ -> return v
  (ReaderT srm) >>= f = ReaderT $  \s -> do v <- srm s
                                            readStateT (f v) s
                                            
instance MonadPlus m => MonadPlus (ReaderT m s) where
  mzero = ReaderT $ \s -> mzero
  (ReaderT p) `mplus` (ReaderT q) = ReaderT $ \s -> p s `mplus` q s                       

instance Monad m => ReaderMonad (ReaderT m s) s where
--  env :: ReaderT m s s
  env = ReaderT $ \s -> return s
-- setenv:: s -> ReaderT m s s -> ReaderT m s s
  setenv s (ReaderT x) = ReaderT $ \_ -> x s
  
instance MonadState m a => MonadState (ReaderT m s) a where
  -- update :: StateT m a => (a -> a) -> ReaderT m s a
  update f = ReaderT $ \_ -> update f

type ParserSimple a = StateT [] String a
  
-- for Parser Char  update f = StateT $ \s -> return (s,f s) 
item1 :: ParserSimple Char
item1 = do {(y:_) <- update tail;return (y)}          

-- -- StateT [] String a


                             
type Parser a = ReaderT (StateT [] Pstring) Pos a
--                       reader's state     
type Pstring = (Pos,String) 
type Pos = (Int,Int)

-- newtype StateT m s a = StateT {runStateT ::(s -> m (a,s))}
-- newtype ReaderT m s a = ReaderT {readStateT::(s -> m a)}


