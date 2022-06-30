{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Types
    ( Info(..)
    , HTML (..)
    , Status(..)
    , SortBy(..)
    , User(..)
    ) where

import Data.Aeson
import Data.Functor.Identity
import Data.SemVer hiding (version)
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Servant.API
import Servant.HTML.Lucid

-- Info
data Info =
    Info
        { version :: Version
        , author :: User
        }
    deriving (Eq, Show, Generic)

instance ToJSON Info

instance FromJSON Info

instance ToJSON Version where
    toJSON v = String . T.pack . toString $ v

instance FromJSON Version where
    parseJSON = withText "version" (return . fromString . T.unpack)

instance IsString Version where
    fromString s =
        case fromText . T.pack $ s of
            Left err -> error err
            Right ver -> ver

instance ToHtml Info where
    toHtml = htmlInfo

    toHtmlRaw = toHtml

htmlInfo :: Monad m => Info -> HtmlT m ()
htmlInfo info = html_ $ do
    head_ $ title_ "Info"
    body_ $ p_ ("v" <> v <> ", " <> a)
  where
    v = toHtml . toString . version $ info
    a = toHtml $ "© " <> (name . author $ info)

-- | Status
data Status
    = Active
    | Inactive

instance FromHttpApiData Status where
    parseQueryParam txt =
        case txt of
            "inactive" -> Right Inactive
            "active" -> Right Active
            _ -> Left "Invalid status"

-- | SortBy
data SortBy
    = Name
    | EMail
    | Date

instance FromHttpApiData SortBy where
    parseQueryParam txt =
        case txt of
            "name" -> Right Name
            "email" -> Right EMail
            "date" -> Right Date
            _ -> Left "Invalid sort-by"

-- | User
data User =
    User
        { name :: String
        , email :: String
        , date :: Maybe Day
        }
    deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User
