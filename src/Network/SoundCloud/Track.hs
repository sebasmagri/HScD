{-# LANGUAGE DeriveGeneric #-}

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

data DownloadJSON = DownloadJSON { download_url     :: String
                                 } deriving (Show, Generic)

instance FromJSON DownloadJSON
instance ToJSON   DownloadJSON

decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

getJSON :: String -> IO (Maybe JSON)
getJSON url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJSON d

decodeDownloadJSON :: String -> Maybe DownloadJSON
decodeDownloadJSON dat = decode (BSL.pack dat) :: Maybe DownloadJSON

decodeComments :: String -> Maybe [Comment.JSON]
decodeComments dat = decode (BSL.pack dat) :: Maybe [Comment.JSON]

getComments :: Int -> IO (Maybe [Comment.JSON])
getComments trackId =
    do let url = tracksURL ++ "/" ++ show trackId ++ "/comments.json?client_id=" ++ clientId
       dat <- scGet url True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeComments d

showComment :: Comment.JSON -> String
showComment c = concat ["\nAt ", Comment.created_at c, ", ", User.username $ Comment.user c, " said:\n", Comment.body c, "\n"]

showComments :: [Comment.JSON] -> String
showComments [] = "No comments"
showComments xs = concatMap showComment xs

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

trackComments :: Int -> IO [Comment.JSON]
trackComments trackId =
    do obj <- getComments trackId
       case obj of
         Nothing -> return []
         Just o  -> return o

showInfo :: String -> IO ()
showInfo trackUrl =
    do obj <- getJSON trackUrl
       case obj of
         Nothing        -> putStrLn "Unable to get track information."
         Just o         ->
             do let tmp = "%s\n%s - %s\n\t%s\nPlays: %d\nComments: %d\nDownloads: %d\nTags:%s\n\nComments:\n%s\n"
                comments <- trackComments $ id o
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
                  (showComments comments)
