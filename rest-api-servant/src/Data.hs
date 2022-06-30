module Data
    ( copyright
    , owner
    ) where

import Control.Monad.Trans.State
import Data.SemVer
import Data.Time.Calendar
import Types

-- Copyright information
--
-- @
-- { "version": "0.0.0"
-- , "authors": [{
--       "name": "Normen Müller",
--       "email": "normen.mueller@gmail.com",
--       "date" : "1976-06-16"
--    }]
-- }
-- @
copyright :: Info
copyright = Info initial [owner]

type Database = State [(Integer, User)] Integer

owner =
    User "Normen Müller" "normen.mueller@gmail.com" (fromGregorian 1976 6 16)
