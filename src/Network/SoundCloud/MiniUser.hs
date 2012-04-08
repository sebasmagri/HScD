{-# LANGUAGE DeriveGeneric #-}

{- |
   Module:      Network.SoundCloud.MiniUser
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   Minimal representation of an user used when embedding users
   information on other resources information
-}

module Network.SoundCloud.MiniUser where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

-- | Represents mini user JSON as a record
data JSON = JSON { id               :: Int
                 , username         :: String
                 , uri              :: String
                 , permalink_url    :: String
                 , avatar_url       :: Maybe String
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

-- | Decode a 'JSON' record from a valid miniuser
-- JSON string
decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON
