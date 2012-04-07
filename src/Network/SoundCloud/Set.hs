{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Set where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Text.Printf (printf)

import qualified Network.SoundCloud.MiniUser as User
import qualified Network.SoundCloud.Track as Track
import Network.SoundCloud.Util (scGet, scResolve)


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
                 , downloadable           :: Bool
                 , playlist_type          :: String
                 , tracks                 :: [Track.JSON]
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

getJSON :: String -> IO (Maybe JSON)
getJSON url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJSON d

showTrack :: Track.JSON -> String
showTrack t = concat ["\t", Track.title t, " by ", User.username $ Track.user t, " (", Track.permalink_url t,")\n"]

showTracks :: [Track.JSON] -> String
showTracks = concatMap showTrack

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
                  (showTracks $ tracks o)
