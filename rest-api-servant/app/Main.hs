module Main where

import API
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Server

main :: IO ()
main = run 8081 app

-- | Web application.
--
-- Turn 'server' into an actual web server using wai and warp.
--
-- 'serve' comes from servant and hands you a WAI Application, which you can
-- think of as an "abstract" web application, not yet a webserver.
app :: Network.Wai.Application
app = Servant.serve api server

api :: Proxy API
api = Proxy
