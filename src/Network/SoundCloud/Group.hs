{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Group where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import qualified Network.SoundCloud.User as User

data JsonRecord = JsonRecord { id                     :: Int
                             , created_at             :: String
                             , permalink_url          :: String
                             , name                   :: String
                             , description            :: String
                             , short_description      :: String
                             , creator                :: User.JsonRecord
                             } deriving (Show, Generic)

instance FromJSON JsonRecord
instance ToJSON   JsonRecord
