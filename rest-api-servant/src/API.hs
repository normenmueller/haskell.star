{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant.API
import Types

type API = InfoAPI -- :<|> UserAPI

type InfoAPI = Get '[JSON] Info

type UserAPI
         -- Create user
    =    "users" :> ReqBody '[JSON] User
                 :> Post '[JSON] User
         -- Read user
    :<|> "users" :> Capture "id" Integer
                 :> Get '[JSON] User
         -- Update user
    :<|> "users" :> Capture "id" Integer
                 :> ReqBody '[JSON] User
                 :> Put '[JSON] User
         -- Query users
    :<|> "users" :> QueryParam "status" Status
                 :> QueryParam "sortby" SortBy
                 :> Get '[JSON] [User]

