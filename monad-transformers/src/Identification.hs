module Identification where

data Input

-- Goal is 'f :: Input -> IO (Maybe Int)'. But IO combined with Maybe is not a
-- monad ... but we want it to! So, let's use monad transformer. So which ones
-- do we need? Let's start with the resulting value:

newtype W =
    W
        { runW :: Input -> IO (Maybe Int)
        }

-- Now, we could either manually ensure that 'W' is a monad ... or ... we
-- identify existing monad transformer.

newtype MaybeT m a =
    MaybeT
        { runMaybeT :: m (Maybe a)
        }

-- So, ...

newtype W' =
    W'
        { runW' :: Input -> MaybeT IO Int
        }

-- and with ...

newtype ReaderT r m a =
    ReaderT
        { runReaderT :: r -> m a
        }

-- we end up with

type W'' = ReaderT Input (MaybeT IO) Int
