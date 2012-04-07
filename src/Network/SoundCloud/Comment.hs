{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Comment where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

import qualified Network.SoundCloud.User as User

data JsonRecord = JsonRecord { id                     :: Int
                             , created_at             :: String
                             , body                   :: String
                             , timestamp              :: Int
                             , user                   :: User.JsonRecord
                             , track_id               :: Int
                             } deriving (Show, Generic)

instance FromJSON JsonRecord
instance ToJSON   JsonRecord

decodeJSON :: String -> Maybe JsonRecord
decodeJSON dat = decode (BSL.pack dat) :: Maybe JsonRecord

showInfo :: String -> IO ()
showInfo trackUrl = putStrLn "Not Implemented"
