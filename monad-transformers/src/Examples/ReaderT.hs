module Examples.ReaderT where

import Control.Monad.Trans.Class

newtype ReaderT r m a =
    ReaderT
        { runReaderT :: r -> m a
        }

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
    pure = ReaderT . pure . pure
    (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmf <*> rma
    -- (<*>) ::      m (a -> b) -> m a -> m b
    -- rmf   :: r -> m (a -> b)
    -- rma   :: r ->               m a
    -- (<$>) :: (a -> b) -> f a -> f b

instance Monad m => Monad (ReaderT r m) where
    return = ReaderT . return . return
    (ReaderT rma) >>= k =
        ReaderT $ \r -> do
            a <- rma r
            runReaderT (k a) r

instance MonadTrans (ReaderT r) where
    lift ma = ReaderT $ const ma 
