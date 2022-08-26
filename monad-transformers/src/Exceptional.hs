{-# LANGUAGE LambdaCase #-}
module Exceptional where

import Control.Monad.IO.Class

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Exception (Exception)

-- |Error.
data Err
    = InvalidArgument
    | Unkown
    deriving Show

instance Exception Err

-- |Effect.
type Eff a = ExceptT Err IO a

-- |Crafted `>>=` for `IO (Either Err a)`. 
bindExceptT ::
       IO (Either Err a) -> (a -> IO (Either Err b)) -> IO (Either Err b)
bindExceptT x f =
    x >>= \case
        Left err -> return $ Left err
        Right val -> f val

readFile' :: FilePath -> Eff String 
readFile' = liftIO . readFile

writeFile' :: FilePath -> String -> Eff ()
writeFile' f = liftIO . writeFile f
