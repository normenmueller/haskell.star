module Examples.EitherT where

import Control.Monad.Trans.Class

newtype EitherT e m a =
    EitherT
        { runEitherT :: m (Either e a)
        }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT x) = EitherT $ (fmap . fmap) f x

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . pure
    (EitherT mmf) <*> (EitherT mma) = EitherT $ (<*>) <$> mmf <*> mma

instance Monad m => Monad (EitherT e m) where
    return = EitherT . return . return
    (EitherT mma) >>= k =
        EitherT $ do
            ma <- mma
            case ma of
                (Right a) -> runEitherT . k $ a
                (Left e) -> return $ Left e

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mma) = EitherT $ go <$> mma
  where
    go :: Either e a -> Either a e
    go (Left e) = Right e
    go (Right x) = Left x

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT lf rf (EitherT mmx) = do
    mx <- mmx
    case mx of
        (Left e) -> lf e
        (Right x) -> rf x

