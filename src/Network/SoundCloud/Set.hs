{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Set where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
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
                             , playlist_type          :: String
                             } deriving (Show, Generic)

instance FromJSON JsonRecord
instance ToJSON   JsonRecord

decodeJSON :: String -> Maybe JsonRecord
decodeJSON dat = decode (BSL.pack dat) :: Maybe JsonRecord

showInfo :: String -> IO ()
showInfo trackUrl = putStrLn "Not Implemented"
