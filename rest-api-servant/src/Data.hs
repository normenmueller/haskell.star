module Data
    ( copyright
    , owner
    ) where

import Control.Monad.Trans.State
import Data.SemVer
import Types

-- Copyright information
--
-- @
-- { "version": "0.0.0"
-- , "authors": [{
--       "name": "Normen Müller",
--       "email": "normen.mueller@gmail.com",
--       "date" : [2022, 1, 1]
--    }]
-- }
-- @
copyright :: Info
copyright = Info initial [owner]

owner = User "Normen Müller" "normen.mueller@gmail.com" (2022, 01, 01)

type Database = State [(Integer, User)] Integer
