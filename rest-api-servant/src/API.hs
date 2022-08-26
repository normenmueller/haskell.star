{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant.API
import Servant.HTML.Lucid
import Types

type API
    =    InfoAPI
    :<|> UserAPI

-- | Info API
type InfoAPI
    -- Get copyright information
     = Get '[ JSON, HTML] Info

-- | User API
--
-- Note: Each occurrence of 'QueryParam', 'Capture', and 'ReqBody' combinators
-- in an endpoint makes the corresponding handler receive an argument of the
-- appropriate type automatically. You don’t have to worry about manually
-- looking up URL captures or query string parameters, or decoding/encoding data
-- from/to JSON. Never.
--
-- Hint: How does servant know to decode, e.g., the 'Integer' from the URL?
-- Keyword: FromHttpApiData and ToHttpApiData.
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
         -- List users
    :<|> "users" :> QueryParam "status" Status
                 :> QueryParam "sortby" SortBy
                 :> Get '[JSON] [User]
