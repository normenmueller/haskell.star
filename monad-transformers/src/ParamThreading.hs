{-# LANGUAGE FlexibleContexts #-}
module ParamThreading where

import Control.Monad.IO.Class
import Control.Monad.Reader

-- The Reader monad, or more generally the MonadReader interface, solves the
-- problem of threading the same configuration to many functions.

-- Imagine this is a directory
type Config = FilePath

loadFile :: Config -> String -> IO String
loadFile path fn = readFile (path ++ fn)

loadRevision :: Config -> Int -> IO String
loadRevision path r = loadFile path ("history" ++ show r ++ ".txt")

loadAll :: Config -> Int -> String -> IO (String, String)
loadAll p r fn = do
    a <- loadFile p fn
    b <- loadRevision p r
    return (a, b)

-- If you look at 'loadAll' you’ll see 'config' is not used, but is threaded
-- through to the child functions. This is a common source of boilerplate and
-- the reader monad attempts to ameliorate it.

-- So instead of threading the 'config' to each function, we can rewrite this
-- using 'MonadReader' and the configuration will get passed implicitly. To
-- retrieve the configuration, we call ask:

loadFile' :: (MonadReader Config m, MonadIO m) => String -> m String
loadFile' fn = do
    path <- ask
    liftIO $ readFile (path ++ fn)

loadRevision' :: (MonadReader Config m, MonadIO m) => Int -> m String
loadRevision' r = loadFile' ("history" ++ show r ++ ".txt")

loadAll' :: (MonadReader Config m, MonadIO m) => Int -> String -> m (String, String)
loadAll' r fn = do
    a <- loadFile' fn
    b <- loadRevision' r
    return (a, b)

-- If you look at the intermediate functions loadRevision and loadAll we no
-- longer have to take in and pass the config around. However the “leaf”
-- function 'load' has gotten more complicated. We will later extend this
-- example to make it reusable across concrete configurations and compare it to
-- alternatives; but first some basics.
