{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.App where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data JsonRecord = JsonRecord { id                     :: Int
                             , permalink_url          :: String
                             , external_url           :: String
                             , creator                :: String
                             } deriving (Show, Generic)

instance FromJSON JsonRecord
instance ToJSON   JsonRecord
