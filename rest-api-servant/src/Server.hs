module Server where

import API
import Data
import Servant.Server
import Types (Info)

server :: Server API
server = info

info :: Handler Info
info = return copyright
