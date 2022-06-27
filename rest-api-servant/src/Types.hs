{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types
    ( Info(..)
    , Status(..)
    , SortBy(..)
    , User(..)
    ) where

import Data.Aeson
import Data.SemVer
import Data.String
import Data.Text
import Data.Text.Encoding
import Data.Time.Calendar
import GHC.Generics

-- Info
data Info =
    Info
        { infoVersion :: Version
        , infoAuthors :: [User]
        }
    deriving (Eq, Show, Generic)

instance ToJSON Info where
    toJSON (Info v as) = object ["version" .= toString v, "authors" .= as]

instance FromJSON Info where
    parseJSON =
        withObject "Info" $ \o -> do
            (v :: String) <- o .: "version"
            as <- o .: "authors"
            return $ Info (fromString v) as

instance IsString Version where
    fromString s =
        case fromText . pack $ s of
            Left err -> error err
            Right ver -> ver

data Status
    = Active
    | Inactive
    | All

data SortBy
    = Date
    | Name

data User =
    User
        { name :: String
        , email :: String
        , date :: (Integer, Int, Int)
        }
    deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User
