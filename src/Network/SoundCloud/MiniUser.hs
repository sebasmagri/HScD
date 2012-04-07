{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.MiniUser where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

data JSON = JSON { id               :: Int
                 , username         :: String
                 , uri              :: String
                 , permalink_url    :: String
                 , avatar_url       :: Maybe String
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON
