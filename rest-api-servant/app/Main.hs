module Main where

import API
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Server

main :: IO ()
main
    -- XXX load and hand over DB to 'app'
    = run 8081 app -- db

-- | Web application.
--
-- Now we can turn 'Server.server' into an actual web server using wai and warp.
--
-- 'serve' comes from servant and hands you a WAI Application, which you can
-- think of as an "abstract" web application, not yet a webserver.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

