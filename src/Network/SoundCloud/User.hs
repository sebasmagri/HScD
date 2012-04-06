{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.User where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data JsonRecord = JsonRecord { id                     :: Int
                             , permalink_url          :: String
                             , country                :: String
                             , full_name              :: String
                             , city                   :: String
                             , description            :: String
                             , website                :: String
                             , website_title          :: String
                             , online                 :: Bool
                             , track_count            :: Int
                             , playlist_count         :: Int
                             , followers_count        :: Int
                             , followings_count       :: Int
                             , public_favorites_count :: Int
                             } deriving (Show, Generic)

instance FromJSON JsonRecord
instance ToJSON   JsonRecord
