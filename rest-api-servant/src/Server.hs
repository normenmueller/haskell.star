module Server where
-- aka. API implementation

import API
import Data
import Servant.API
import Servant.Server
import Types

-- | Server.
--
-- This is the actual web service that will handle the API requests. The type of
-- the web application (cf. Main#app) is determined by the API type (through a
-- type family (cf. "Haskell's Type Families"; ADD ~ Server).
--
-- The first thing to know about the Server type family is that behind the
-- scenes it will drive the routing, letting you focus only on the business
-- logic.
--
-- The second thing to know is that for each endpoint, your handlers will
-- by default run in the Handler monad. This is overridable very easily.
--
-- Third thing, the type of the value returned in that monad must be the same as
-- the second argument of the HTTP method combinator used for the corresponding
-- endpoint.
--
-- Note: Handlers must be provided in the same order as in the API type.
server :: Server API
server = info
    :<|> createUser
    :<|> readUser
    :<|> updateUser
    :<|> listUsers

info :: Handler Info
info = return copyright

createUser :: User -> Handler User
createUser = return . const owner

readUser :: Integer -> Handler User
readUser = return . const owner

updateUser :: Integer -> User -> Handler User
updateUser uid = return

listUsers :: Maybe Status -> Maybe SortBy -> Handler [User]
listUsers ms mo = return [owner]
