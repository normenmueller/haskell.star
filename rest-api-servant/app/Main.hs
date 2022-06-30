module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API
import Server

main :: IO ()
main = run 8081 app

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

