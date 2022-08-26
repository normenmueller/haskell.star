{-# LANGUAGE OverloadedStrings #-}
module Data where

import Data.Maybe
import Data.SemVer
import Data.Time.Calendar
import Types

-- Copyright information
--
-- @
-- { "version": "0.1.0"
-- , "author": {
--       "name": "Normen Müller",
--       "email": "normen.mueller@gmail.com",
--       "date" : "1976-06-16"
--    }
-- }
-- @
copyright :: Info
copyright = Info "0.1.0" owner

-- | Copyright holder
owner :: User
owner =
    User
        "Normen Müller"
        "normen.mueller@gmail.com"
        (Just $ fromGregorian 1976 6 16)

-- | A user identifier.
type UserId = Integer

-- | A database.
type Database = [(UserId, User)]

-- | Primitive database mock-up.
mockDb :: Database
mockDb = undefined
