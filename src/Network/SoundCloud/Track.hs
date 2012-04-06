{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Track where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import qualified Network.SoundCloud.User as User

data JsonRecord = JsonRecord { id                     :: Int
                             , created_at             :: String
                             , user                   :: User.JsonRecord
                             , title                  :: String
                             , permalink_url          :: String
                             , sharing                :: String
                             , description            :: String
                             , label                  :: User.JsonRecord
                             , duration               :: Int
                             , genre                  :: String
                             , shared_to_count        :: Int
                             , license                :: String
                             , release_day            :: Int
                             , release_month          :: Int
                             , release_year           :: Int
                             , streamable             :: Bool
                             , downloadable           :: Bool
                             , track_type             :: String
                             , download_url           :: String
                             , stream_url             :: String
                             , bpm                    :: Int
                             , comment_count          :: Int
                             , download_count         :: Int
                             , playback_count         :: Int
                             , favoritings_count      :: Int
                             , original_format        :: String
                             , original_content_size  :: Int
--                             , created_with           :: App.JsonRecord
                             } deriving (Show, Generic)

instance FromJSON JsonRecord
instance ToJSON   JsonRecord
