{-# LANGUAGE DeriveGeneric #-}

{- |
   Module:      Network.SoundCloud.Set
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   Representations for track sets
-}

module Network.SoundCloud.Set where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Text.Printf (printf)

import qualified Network.SoundCloud.MiniUser as User
import qualified Network.SoundCloud.Track as Track
import Network.SoundCloud.Util (scGet, scResolve)

-- | Represents Set JSON as a record
data JSON = JSON { id                     :: Int
                 , created_at             :: String
                 , user                   :: User.JSON
                 , title                  :: String
                 , permalink_url          :: String
                 , sharing                :: String
                 , description            :: String
                 , duration               :: Int
                 , genre                  :: String
                 , license                :: String
                 , release_day            :: Maybe Int
                 , release_month          :: Maybe Int
                 , release_year           :: Maybe Int
                 , streamable             :: Bool
                 , downloadable           :: Maybe Bool
                 , playlist_type          :: String
                 , tracks                 :: [Track.JSON]
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

-- | Decode a JSON record from a valid set
-- JSON string
decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

-- | Get a set JSON record given it's public URL
-- as in http://soundcloud.com/artist/set_title
getJSON :: String -> IO (Maybe JSON)
getJSON url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJSON d

-- | Get a string with a summary description of a track
showTrack :: Track.JSON -> String
showTrack t = concat ["\t", Track.title t, " by ", User.username $ Track.user t, " (", Track.permalink_url t,")\n"]

-- | Show general information about a set in the
-- standard output
showInfo :: String -> IO ()
showInfo url =
    do obj <- getJSON url
       case obj of
         Nothing        -> putStrLn "Unable to get set information."
         Just o         ->
             do let tmp = "%s\n%s by %s (%s)\n\t%s\nCreated at: %s\n\nTracks:\n%s\n"
                printf
                  tmp
                  (permalink_url o)
                  (title o)
                  (User.username $ user o)
                  (genre o)
                  (description o)
                  (created_at o)
                  (concatMap showTrack $ tracks o)
