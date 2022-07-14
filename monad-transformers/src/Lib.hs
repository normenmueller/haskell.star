{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib
    ( LM
    , incAll
    , run
    ) where

import Control.Monad.IO.Class

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Control.Monad.Trans.Class as T
import qualified Control.Monad.Trans.Except as T
import qualified Control.Monad.Trans.Identity as T
import qualified Control.Monad.Trans.Maybe as T
import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.State.Lazy as T
import Data.Functor.Identity (Identity)


--newtype MaybeT m a =
--    MaybeT
--        { runMaybeT :: m (Maybe a)
--        }
--
--instance (Functor m) => Functor (MaybeT m) where
--    fmap f = MaybeT . (fmap . fmap $ f) . runMaybeT
--
--instance (Functor m, Monad m) => Applicative (MaybeT m) where
--    pure = MaybeT . return . return
--
--    f <*> fa =
--        MaybeT $ do
--            f' <- runMaybeT f
--            a' <- runMaybeT fa
--            return $ f' <*> a'

--data Env deriving Show
--data Status deriving Show
--data Error deriving Show

-- | The pattern we're trying to abstact
--   'pattern :: Env -> Status -> IO (Either Error a)'
--   Which can be done ...
--runApp :: Env -> Status -> IO (Either Error a)
--runApp e s = T.runExceptT (T.evalStateT (T.runReaderT app e) s)

-- ... using three monad transformers:
--app :: T.ReaderT Env (T.StateT Status (T.ExceptT Error IO)) a
--app = undefined

--type App a = T.ReaderT Env (T.StateT Status (T.ExceptT Error IO)) a

-- Usage examples

-- !!! w/o mtl
--printEnv :: App ()
--printEnv = T.ask >>= liftIO . print

-- !!! w/ mtl
--printEnv' :: App ()
--printEnv' = ask >>= liftIO . print

-- !!! w/o mtl
--printStatus :: App ()
--printStatus = do
--    _ <- T.ask 
--    s <- T.lift T.get 
--    liftIO . print $ s

-- !!! w/ mtl
--printStatus' :: App ()
--printStatus' = get >>= liftIO . print

type Status s = StatusT s Identity

newtype StatusT s m a =
    StatusT
        { runStatusT :: s -> m (a, s)
        }

instance (Functor m) => Functor (MaybeT m) where
    fmap f = MaybeT . (fmap . fmap $ f) . runMaybeT

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . return . return
    f <*> fa =
        MaybeT $ do
            f' <- runMaybeT f
            a' <- runMaybeT fa
            return $ f' <*> a'

x = OptionT . W . Just $ 5

-- Angenommen man will eine 'Maybe' Monade und einen '[]' Monade kombinieren,
-- wobei die Kombination---das Result---auch eine Instanz der cc of monads
-- darstellen soll.
--
-- Ziel: instance Monad [Maybe a]
--
-- Hier kommen monad transformers zur Hilfe!

type LM a = T.MaybeT [] a

incAll :: Num a => LM a -> LM a
incAll = fmap (1+)

run :: LM a -> [Maybe a]
run = T.runMaybeT 
