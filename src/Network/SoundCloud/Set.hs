{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Set where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

import qualified Network.SoundCloud.MiniUser as User

data JSON = JSON { id                     :: Int
                 , created_at             :: String
                 , user                   :: User.JSON
                 , title                  :: String
                 , permalink_url          :: String
                 , sharing                :: String
                 , description            :: String
                 , label                  :: User.JSON
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

instance FromJSON JSON
instance ToJSON   JSON

decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

showInfo :: IO ()
showInfo = putStrLn "Not Implemented for sets"
