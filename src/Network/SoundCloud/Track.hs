{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.Track where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Text.Printf (printf)

import Network.SoundCloud.Util (scGet, scFetch, scResolve)
import Network.SoundCloud.Const (clientId)
import qualified Network.SoundCloud.MiniUser as User

data Json = Json { id                     :: Int
                 , created_at             :: String
                 , user                   :: User.JsonRecord
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

instance FromJSON Json
instance ToJSON   Json

data DownloadJson = DownloadJson { download_url     :: String
                                 } deriving (Show, Generic)

instance FromJSON DownloadJson
instance ToJSON   DownloadJson

decodeJson :: String -> Maybe Json
decodeJson dat = decode (BSL.pack dat) :: Maybe Json

getJson :: String -> IO (Maybe Json)
getJson url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJson d

decodeDownloadJson :: String -> Maybe DownloadJson
decodeDownloadJson dat = decode (BSL.pack dat) :: Maybe DownloadJson

fetch :: String -> String -> IO ()
fetch trackUrl output =
    do tUrl <- scResolve trackUrl
       dat <- scGet tUrl True
       case dat of
         Nothing -> putStrLn "Unable to connect"
         Just d  ->
             do let o = decodeJson d
                case o of
                  Nothing        -> putStrLn "Unable to get track information."
                  Just obj       ->
                      if downloadable obj then
                          do let obj0 = decodeDownloadJson d
                             let dUrlStr = concat [download_url $ fromJust obj0, "?client_id=", clientId]
                             let filename = if null output then
                                               "./" ++ title obj ++ "." ++ original_format obj
                                            else output
                             putStrLn $ "Fetching " ++ show (original_content_size obj) ++ " bytes"
                             scFetch dUrlStr filename
                      else putStrLn "Track is not downloadable"

showInfo :: String -> IO ()
showInfo trackUrl =
    do obj <- getJson trackUrl
       case obj of
         Nothing        -> putStrLn "Unable to get track information."
         Just o         ->
             do let tmp = "%s\n%s - %s\n\t%s\nPlays: %d\nComments: %d\nDownloads: %d\nTags:%s\n"
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
