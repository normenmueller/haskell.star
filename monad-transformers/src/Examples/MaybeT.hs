module Examples.MaybeT where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype MaybeT m a =
    MaybeT
        { runMaybeT :: m (Maybe a)
        }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (MaybeT f) <*> (MaybeT a) = MaybeT $ (<*>) <$> f <*> a

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . return
    (MaybeT mma) >>= k =
        MaybeT $ do
            ma <- mma
            case ma of
                (Just a) -> runMaybeT . k $ a
                Nothing -> return Nothing

instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
