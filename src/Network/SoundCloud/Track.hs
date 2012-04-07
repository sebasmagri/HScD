{-# LANGUAGE DeriveGeneric #-}

{- |
   Module:      Network.SoundCloud.Track
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   Implements tracks and related types and functions
-}

module Network.SoundCloud.Track where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Prelude hiding (id)
import Text.Printf (printf)

import Network.SoundCloud.Util (scGet, scFetch, scResolve)
import Network.SoundCloud.Const (clientId, tracksURL)
import qualified Network.SoundCloud.MiniUser as User
import qualified Network.SoundCloud.Comment as Comment

-- | Represent a track's JSON as a record
data JSON = JSON { id                     :: Int
                 , created_at             :: String
                 , user                   :: User.JSON
                 , title                  :: String
                 , permalink_url          :: String
                 , sharing                :: String
                 , description            :: String
                 , label_id               :: Maybe Int
                 , label_name             :: Maybe String
                 , duration               :: Int
                 , genre                  :: String
                 , license                :: String
                 , release_day            :: Maybe Int
                 , release_month          :: Maybe Int
                 , release_year           :: Maybe Int
                 , streamable             :: Bool
                 , downloadable           :: Bool
                 , track_type             :: String
                 , stream_url             :: String
                 , bpm                    :: Maybe Int
                 , comment_count          :: Int
                 , download_count         :: Int
                 , playback_count         :: Int
                 , favoritings_count      :: Int
                 , original_format        :: String
                 , original_content_size  :: Int
                 , tag_list               :: String
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

-- | Simple record to parse download_url from a track's JSON
data DownloadJSON = DownloadJSON { download_url     :: String
                                 } deriving (Show, Generic)

instance FromJSON DownloadJSON
instance ToJSON   DownloadJSON

-- | Decode a JSON record from a track valid JSON string
decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

-- | Get a JSON record given a track URL
-- as in http://soundcloud.com/artist/track_title
getJSON :: String -> IO (Maybe JSON)
getJSON url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJSON d

-- | Decode a DownloadJSON record out of a track's JSON
decodeDownloadJSON :: String -> Maybe DownloadJSON
decodeDownloadJSON dat = decode (BSL.pack dat) :: Maybe DownloadJSON

-- | Decode a comment JSON list given a track id
decodeComments :: String -> Maybe [Comment.JSON]
decodeComments dat = decode (BSL.pack dat) :: Maybe [Comment.JSON]

-- | Given the track id, get its comments as a list of Comment.JSON
getComments :: Int -> IO (Maybe [Comment.JSON])
getComments trackId =
    do let url = tracksURL ++ "/" ++ show trackId ++ "/comments.json?client_id=" ++ clientId
       dat <- scGet url True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeComments d

-- | Fetch a downloadable track
fetch :: String -> String -> IO ()
fetch trackUrl output =
    do tUrl <- scResolve trackUrl
       dat <- scGet tUrl True
       case dat of
         Nothing -> putStrLn "Unable to connect"
         Just d  ->
             do let o = decodeJSON d
                case o of
                  Nothing        -> putStrLn "Unable to get track information."
                  Just obj       ->
                      if downloadable obj then
                          do let obj0 = decodeDownloadJSON d
                             let dUrlStr = concat [download_url $ fromJust obj0, "?client_id=", clientId]
                             let filename = if null output then
                                               "./" ++ title obj ++ "." ++ original_format obj
                                            else output
                             putStrLn $ "Fetching " ++ show (original_content_size obj) ++ " bytes"
                             scFetch dUrlStr filename
                      else putStrLn "Track is not downloadable"

-- | Show general information about the track at the given URL
-- in the standard output
showInfo :: String -> IO ()
showInfo trackUrl =
    do obj <- getJSON trackUrl
       case obj of
         Nothing        -> putStrLn "Unable to get track information."
         Just o         ->
             do let tmp = "%s\n%s - %s\n\t%s\nPlays: %d\nComments: %d\nDownloads: %d\nTags:%s\n\nComments:\n%s\n"
                comments <- getComments $ id o
                let commentsList = fromJust comments
                printf
                  tmp
                  (permalink_url o)
                  (title o)
                  (User.username $ user o)
                  (description o)
                  (playback_count o)
                  (comment_count o)
                  (download_count o)
                  (tag_list o)
                  (if not $ null commentsList then concatMap Comment.showComment commentsList else "No comments")
