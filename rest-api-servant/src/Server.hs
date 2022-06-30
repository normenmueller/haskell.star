module Server where

import API
import Data
import Servant.API
import Servant.Server
import Types

server :: Server API
server = info
    :<|> createUser
    :<|> readUser
    :<|> updateUser
    :<|> listUsers

info :: Handler Info
info = return copyright

createUser :: User -> Handler User 
createUser = undefined

readUser :: Integer -> Handler User 
readUser = undefined

updateUser :: Integer -> User -> Handler User 
updateUser = undefined

listUsers :: Maybe Status -> Maybe SortBy -> Handler [User]
listUsers = undefined
