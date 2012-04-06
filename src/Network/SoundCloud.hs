{-# LANGUAGE DeriveGeneric #-}

{-
  Implementing the SoundCloud API
-}
module Network.SoundCloud where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Network.HTTP
import System.IO


data ScJSON = ScJSON { id                       :: Int
                       , user_id                :: Int
                       , original_content_size  :: Int
                       , downloadable           :: Bool
                       , title                  :: String
                       , original_format        :: String
                       , download_url           :: String
                       } deriving (Show, Generic)

instance FromJSON ScJSON
instance ToJSON   ScJSON


clientId :: String
clientId = "934a79db328a60a0ea459ab9e45c1735"

apiURL :: String
apiURL = "http://api.soundcloud.com"

apiURLS :: String
apiURLS = "https://api.soundcloud.com"

apiMeURLS :: String
apiMeURLS = apiURLS ++ "/me"

authURLS :: String
authURLS = "https://soundcloud.com/connect"

tokenURLS :: String
tokenURLS = apiURLS ++ "/oauth2/token"

tracksURL :: String
tracksURL = apiURL ++ "/tracks"

usersURL :: String
usersURL = apiURL ++ "/users"

playlistsURL :: String
playlistsURL = apiURL ++ "/playlists"

groupsURL :: String
groupsURL = apiURL ++ "/groups"

commentsURL :: String
commentsURL = apiURL ++ "/comments"

meConnectionsURLS :: String
meConnectionsURLS = apiMeURLS ++ "/connections"

meActivitiesURLS :: String
meActivitiesURLS = apiMeURLS ++ "/activities"

appsURLS :: String
appsURLS = apiURL ++ "/apps"

resolveURL :: String
resolveURL = apiURL ++ "/resolve"

scGet :: String -> Bool -> IO (Maybe String)
scGet url followRedirections =
    do res <- simpleHTTP $ getRequest url
       case res of
         Left   _ -> return Nothing
         Right  r ->
             case rspCode r of
               (2,_,_) -> return $ Just $ rspBody r
               (3,_,_) ->
                   case findHeader HdrLocation r of
                     Nothing       -> return Nothing
                     Just uri      ->
                         if followRedirections
                         then scGet uri True
                         else return $ Just uri
               _ -> return Nothing

scGetJSON :: String -> IO String
scGetJSON url =
    do result <- scGet url False
       case result of
         Nothing    -> return "{}"
         Just     r -> return r

scFetch :: String -> String -> IO ()
scFetch dUrl out =
    do contents <- scGet dUrl True
       case contents of
         Nothing -> putStrLn "Could not fetch file contents."
         Just  c ->
             do file <- openBinaryFile out WriteMode
                hPutStr file c
                hClose file


scFetchTrack :: String -> String -> IO ()
scFetchTrack trackUrl output =
    do tUrl <- scResolve trackUrl
       dat <- scGetJSON tUrl
       let o = decode (BSL.pack dat) :: Maybe ScJSON
       case o of
         Nothing        -> putStrLn "Unable to get track information."
         Just obj       ->
             if downloadable obj then
                 do let dUrlStr = concat [download_url obj, "?client_id=", clientId]
                    let filename = if null output then "./" ++ title obj ++ "." ++ original_format obj else output
                    putStrLn $ "Fetching " ++ show (original_content_size obj) ++ " bytes"
                    scFetch dUrlStr filename
             else putStrLn "Track is not downloadable"


{-
This function's request will always return a (3,_,_) status,
so we can return the redirection Location
-}
scResolve :: String -> IO String
scResolve url =
    do dat <- scGet resolveUrl False
       case dat of
         Nothing        -> return ""
         Just d         -> return d
    where
        resolveUrl = concat [resolveURL, ".json?url=", url, "&client_id=", clientId]

