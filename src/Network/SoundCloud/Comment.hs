{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Comment where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

import qualified Network.SoundCloud.MiniUser as User

data Json = Json { id                     :: Int
                 , created_at             :: String
                 , body                   :: String
                 , timestamp              :: Maybe Int
                 , user                   :: User.JsonRecord
                 , track_id               :: Int
                 } deriving (Show, Generic)

instance FromJSON Json
instance ToJSON   Json

decodeJson :: String -> Maybe Json
decodeJson dat = decode (BSL.pack dat) :: Maybe Json
