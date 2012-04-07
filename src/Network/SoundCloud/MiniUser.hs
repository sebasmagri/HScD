{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.MiniUser where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

data JsonRecord = JsonRecord { id               :: Int
                             , username         :: String
                             , uri              :: String
                             , permalink_url    :: String
                             , avatar_url       :: Maybe String
                             } deriving (Show, Generic)

instance FromJSON JsonRecord
instance ToJSON   JsonRecord

decodeJSON :: String -> Maybe JsonRecord
decodeJSON dat = decode (BSL.pack dat) :: Maybe JsonRecord
