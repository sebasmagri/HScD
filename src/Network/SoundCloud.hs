{-# LANGUAGE DeriveGeneric #-}

{-
  Implementing the SoundCloud API
-}
module Network.SoundCloud where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List
import Network.HTTP
import System.IO

import qualified Network.SoundCloud.Track as Track
import qualified Network.SoundCloud.User as User
import qualified Network.SoundCloud.Set as Set
import qualified Network.SoundCloud.Group as Group
import qualified Network.SoundCloud.Comment as Comment
import qualified Network.SoundCloud.App as App

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
    do dat <- scGetInfo trackUrl
       let o = decode (BSL.pack dat) :: Maybe Track.JsonRecord
       case o of
         Nothing        -> putStrLn "Unable to get track information."
         Just obj       ->
             if Track.downloadable obj then
                 do let dUrlStr = concat [Track.download_url obj, "?client_id=", clientId]
                    let filename = if null output then
                                       "./" ++ Track.title obj ++ "." ++ Track.original_format obj
                                   else output
                    putStrLn $ "Fetching " ++ show (Track.original_content_size obj) ++ " bytes"
                    scFetch dUrlStr filename
             else putStrLn "Track is not downloadable"


{-
This function's request will always return a (3,_,_) status,
so we can just return the redirection Location
-}
scResolve :: String -> IO String
scResolve url =
    do dat <- scGet resolveUrl False
       case dat of
         Nothing        -> return ""
         Just d         -> return d
    where
        resolveUrl = concat [resolveURL, ".json?url=", url, "&client_id=", clientId]

scResourceType :: String -> String
scResourceType url | tracksURL    `isPrefixOf` url      = "track"
                   | usersURL     `isPrefixOf` url      = "user"
                   | playlistsURL `isPrefixOf` url      = "set"
                   | groupsURL    `isPrefixOf` url      = "group"
                   | commentsURL  `isPrefixOf` url      = "comment"
                   | appsURLS     `isPrefixOf` url      = "app"
scResourceType _                                        = ""


{-
Get the information about an arbitrary object given its URL
-}
scGetInfo :: String -> IO String
scGetInfo url =
    do tUrl <- scResolve url
       dat  <- scGetJSON tUrl
       putStrLn $ scResourceType tUrl
       return dat
