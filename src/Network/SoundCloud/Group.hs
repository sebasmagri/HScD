{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Group where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

import qualified Network.SoundCloud.MiniUser as User

data JSON = JSON { id                     :: Int
                 , created_at             :: String
                 , permalink_url          :: String
                 , name                   :: String
                 , description            :: String
                 , short_description      :: String
                 , creator                :: User.JSON
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

showInfo :: IO ()
showInfo = putStrLn "Not Implemented for groups"
