module Introductions where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

data Input

-- Goal is 'app :: Input -> IO (Maybe Int)'. This is what we actually want!
-- This is the type of the ultimativelly computed value of the application (cf.
-- 'runApp')! But the combination of 'IO' and 'Maybe' is not a monad in general!
-- Cf. 'Compostion#Compose'. So we could define our handcrafted one. Cf.
-- 'Compose#MaybeIO'. But, then we would have to make one-off types for each
-- combination! That's tiresome :(
--
-- So ... let's use monad transformer!
--
-- ... but which ones do we need? Let's start with the resulting value:

newtype W =
    W
        { runW :: Input -> IO (Maybe Int)
        --                 ^^^^^^^^^^^^^^
        --                 the ultimativelly computed value
        }

-- Now, we could either manually ensure that 'W' is a monad ... or ... we
-- identify existing monad transformer: 'MaybeT'

newtype W' =
    W'
        { runW' :: Input -> MaybeT IO Int
        }

-- and 'ReaderT', we end up with

type App a = ReaderT Input (MaybeT IO) Int

runApp :: App a -> Input -> IO (Maybe Int)
runApp app = runMaybeT . runReaderT app
