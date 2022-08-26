{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Crafted where

import qualified Control.Monad.State as MTL
import Control.Monad.State (MonadState(..))

import Control.Monad
import Control.Monad.Trans.Class
--import Control.Monad.Trans.Except
--import Control.Monad.Trans.State

import Data.Bifunctor
import Data.Functor.Identity
import GHC.Base (undefined)

-- ExceptT {{{1

newtype ExceptT e m a =
    ExceptT
        { runExceptT :: m (Either e a)
        }

instance Functor m => Functor (ExceptT e m) where
    fmap f = ExceptT . (fmap . fmap) f . runExceptT

instance (Functor m, Monad m) => Applicative (ExceptT e m) where
    pure = ExceptT . return . Right
    mf <*> mx = ExceptT $ do
        f <- runExceptT mf
        x <- runExceptT mx
        return $ f <*> x

instance Monad m => Monad (ExceptT e m) where
    return = pure
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> runExceptT (k x)

instance MonadTrans (ExceptT e) where
    lift = ExceptT . liftM Right

instance MonadState s m => MonadState s (ExceptT e m) where
    get = lift get
    put = lift . put
    state = lift . state

-- StateT {{{1

newtype StateT s m a =
    StateT
        { runStateT :: s -> m (a, s)
        }

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ fmap (first f) . runStateT m

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
    return = pure
    m >>= k = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'

instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)
    state f = StateT $ return . f

-- | This is what we actually want:
--
-- @
-- Integer -> IO (Either String a, Integer)
-- @
--
-- This is the type of the ultimativelly computed value of the application (cf.
-- 'runApp')!
type App a = ExceptT String (StateT Integer IO) a

app :: App Integer
app = return 1

-- | Run application.
--
-- @
-- main :: IO ()
-- main = do
--     result <- runApp app 0
--     print result
-- @
runApp :: App a -> Integer -> IO (Either String a, Integer)
runApp c = runStateT (runExceptT c)

-- |'runApp' without state.
runApp' :: App a -> Integer -> IO (Either String a)
runApp' c = fmap fst . runApp c
