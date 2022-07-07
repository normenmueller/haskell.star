module Lib
    ( MaybeT
    , runMaybeT
    ) where

newtype MaybeT m a =
    MaybeT
        { runMaybeT :: m (Maybe a)
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
