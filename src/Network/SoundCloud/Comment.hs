{-# LANGUAGE DeriveGeneric #-}

{- |
   Module:      Network.SoundCloud.Comment
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   Represents SoundCloud comments
-}

module Network.SoundCloud.Comment where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)

import qualified Network.SoundCloud.MiniUser as User

-- | Represents comment's JSON as a record
data JSON = JSON { id                     :: Int
                 , created_at             :: String
                 , body                   :: String
                 , timestamp              :: Maybe Int
                 , user                   :: User.JSON
                 , track_id               :: Int
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

-- | Decode a comment's valid JSON string into
-- a comment's JSON record
decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

-- | Show a summary about a comment
showComment :: JSON -> String
showComment c = concat ["\nAt ", created_at c, ", ", User.username $ user c, " said:\n", body c, "\n"]
