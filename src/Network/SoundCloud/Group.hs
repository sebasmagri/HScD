{-# LANGUAGE DeriveGeneric #-}

{- |
   Module:      Network.SoundCloud.Group
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   Represents Groups as in http://soundcloud.com/groups
-}

module Network.SoundCloud.Group where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Text.Printf (printf)

import qualified Network.SoundCloud.MiniUser as User
import Network.SoundCloud.Util (scGet, scResolve)

-- | Represents group's JSON as a record
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

-- | Decode a group's valid JSON string into a record
decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

-- | Get a group record given a public URL
-- as in http://soundcloud.com/groups/group_name
getJSON :: String -> IO (Maybe JSON)
getJSON url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJSON d

-- | Show general information about a group in the
-- standard output
showInfo :: String -> IO ()
showInfo url =
    do obj <- getJSON url
       case obj of
         Nothing        -> putStrLn "Unable to get track information."
         Just o         ->
             do let tmp = "%s\n%s (Since %s)\n\t%s\nCreated by %s\n"
                printf
                  tmp
                  (permalink_url o)
                  (name o)
                  (created_at o)
                  (description o)
                  (User.username $ creator o)
