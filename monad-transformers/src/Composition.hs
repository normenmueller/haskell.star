{-# LANGUAGE InstanceSigs #-}
module Composition where

-- cf. HPFFP#963ff

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

-- There’s no problem composing two arbitrary datatypes that have Monad
-- instances. However, the result of having done so does not give you a Monad.
--
-- !!! Getting another Monad given the composition of two *arbitrary* types that
-- have a Monad instance is impossible!

instance (Monad f, Monad g) => Monad (Compose f g) where
    return = pure

    (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
    (Compose fga) >>= k =
          --xxx :: f (g (f (g b)))
        let xxx = (fmap . fmap) (runCompose . k) fga
         in undefined -- no chance to massage 'xxx' any furhter :(

-- The fundamental problem with composing two monads lies in the impossibility
-- of joining two *unknown* monads.
--
-- In order to make that join happen, we need to reduce the polymorphism and get
-- concrete information about one of the monads that we’re working with

newtype MaybeIO a =
    MaybeIO
        { runMaybeIO :: IO (Maybe a)
        }

instance Functor MaybeIO where
    fmap f x =
        MaybeIO $ do
            ma <- runMaybeIO x
            return $ f <$> ma

instance Applicative MaybeIO where
    pure = MaybeIO . pure . pure
    (MaybeIO imf) <*> (MaybeIO ima) =
        MaybeIO $ do
            f <- imf
            a <- ima
            return $ f <*> a

instance Monad MaybeIO where
    return = pure

    (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    (MaybeIO ima) >>= k =
        -- In 'xxx' we do not use any concrete information of any of the two
        -- monads to be combined, but handle both as arbitray monads.
        let xxx = (fmap . fmap) (runMaybeIO . k) ima
        -- But, remember, without concrete information about, at least, one of
        -- the two monads to be combined, joining 'f (g (f (g a)))' is
        -- impossible!
        -- So let's use concrete information:
         in MaybeIO $ do
                -- here we still only use the information that 'ima' is an
                -- arbitray monad  ...
                ma <- ima
                -- ... but, eventually, here we use concrete information!
                case ma of
                    Just a -> runMaybeIO $ k a
                    Nothing -> return Nothing
