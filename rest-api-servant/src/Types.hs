{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
        withObject "Info" $ \o -> Info
            <$> (fromString <$> (o .: "version"))
            <*> o .: "authors"

instance IsString Version where
    fromString s =
        case fromText . pack $ s of
            Left err -> error err
            Right ver -> ver

-- | Status
data Status
    = Active
    | Inactive
    | All

-- | SortBy
data SortBy
    = Date
    | Name

-- | User
data User =
    User
        { name :: String
        , email :: String
        , date :: Day
        }
    deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User
