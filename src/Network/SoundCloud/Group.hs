{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Group where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
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

decodeJSON :: String -> Maybe JsonRecord
decodeJSON dat = decode (BSL.pack dat) :: Maybe JsonRecord

showInfo :: String -> IO ()
showInfo trackUrl = putStrLn "Not Implemented"
