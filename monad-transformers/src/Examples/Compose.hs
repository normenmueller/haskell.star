{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
module Examples.Compose where

import Control.Applicative (liftA2, Alternative (many))

{- Identity -}

newtype Identity (a :: *) =
    Identity
        { runIdentity :: a
        }

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (Identity f) <*> (Identity a) = Identity $ f a

-- |Compose.
-- A datatype that corresponds to function composition.
newtype Compose (f :: * -> *) (g :: * -> *) (a :: *) =
    Compose
        { runCompose :: f (g a)
        }
    deriving (Eq, Show)

-- Functors are closed under composition!
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose a) = Compose $ (fmap . fmap) f a

-- Applicatives are closed under compositon!
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = undefined -- Compose . pure . pure
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose k) <*> (Compose x) =
        Compose $
        liftA2 -- here we use the context 'Applicative f'
            ((<*>) :: Applicative h => -- here we use the context 'Applicative g'
                          h (a -> b) -> h a -> h b)
            k   -- :: f  (g (a -> b)))
            x   -- :: f                (g a)

-- Foldables are closed under composition!
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap f (Compose x) = (foldMap . foldMap) f x

-- Traversables are closed under composition!
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
    traverse f (Compose x) = Compose <$> (traverse . traverse) f x
