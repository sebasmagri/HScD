{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Comment where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

import qualified Network.SoundCloud.MiniUser as User

data JSON = JSON { id                     :: Int
                 , created_at             :: String
                 , body                   :: String
                 , timestamp              :: Maybe Int
                 , user                   :: User.JSON
                 , track_id               :: Int
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON
