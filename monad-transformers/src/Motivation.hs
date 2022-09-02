{-# LANGUAGE InstanceSigs #-}
module Motivation where

-- cf. HPFFP#963ff

newtype Identity a =
    Identity
        { runIdentity :: a
        }

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

newtype IdentityT m a =
    IdentityT
        { runIdentityT :: m a
        }

instance (Functor f) => Functor (IdentityT f) where
    fmap f x = IdentityT $ f <$> runIdentityT x

instance (Applicative f) => Applicative (IdentityT f) where
    pure = IdentityT . pure
    (IdentityT f) <*> (IdentityT a) = IdentityT $ f <*> a

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    m >>= k = IdentityT $ runIdentityT m >>= runIdentityT . k

newtype Compose f g a =
    Compose
        { runCompose :: f (g a)
        }

-- Composing two functors yields another functor! Ie., functors are closed under
-- composition.
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Composing two applicatives yields another applicative! Ie., applicatives are
-- closed under composition.
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure

    -- cf. https://stacktracehq.com/blog/applicative-instance-for-compose/
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose x) = Compose $ (<*>) <$> f <*> x
      where
        --tmp0 :: f (g a -> g b)
        tmp0 = (<*>) <$> f
        --x :: f (g a)
        tmp1 = tmp0 <*> x
