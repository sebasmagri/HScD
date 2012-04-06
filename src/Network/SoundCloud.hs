{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Implementing the SoundCloud API
-}
module Network.SoundCloud where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as StrLazy
import qualified Data.Text as Txt
import GHC.Generics (Generic)
import Network.HTTP
import System.IO


data ScUser = ScUser {
    userId               :: Int
  , userPermalink        :: Txt.Text
  , userName             :: Txt.Text
  , userUri              :: Txt.Text
  , userPermalinkUrl     :: Txt.Text
  , userAvatarUrl        :: Txt.Text
  } deriving (Show, Generic)

data ScTrack = ScTrack { id                       :: Int
                       , user_id                  :: Int
                       , original_content_size :: Int
                       , downloadable        :: Bool
                       , title               :: Txt.Text
                       , original_format      :: Txt.Text
                       , download_url          :: Txt.Text
                       } deriving (Show, Generic)

instance FromJSON ScUser
instance ToJSON   ScUser
instance FromJSON ScTrack
instance ToJSON   ScTrack


clientId :: Txt.Text
clientId = "934a79db328a60a0ea459ab9e45c1735"

apiURL :: Txt.Text
apiURL = "http://api.soundcloud.com"

apiURLS :: Txt.Text
apiURLS = "https://api.soundcloud.com"

apiMeURLS :: Txt.Text
apiMeURLS = Txt.append apiURLS "/me"

authURLS :: Txt.Text
authURLS = "https://soundcloud.com/connect"

tokenURLS :: Txt.Text
tokenURLS = Txt.append apiURLS "/oauth2/token"

tracksURL :: Txt.Text
tracksURL = Txt.append apiURL "/tracks"

usersURL :: Txt.Text
usersURL = Txt.append apiURL "/users"

playlistsURL :: Txt.Text
playlistsURL = Txt.append apiURL "/playlists"

groupsURL :: Txt.Text
groupsURL = Txt.append apiURL "/groups"

commentsURL :: Txt.Text
commentsURL = Txt.append apiURL "/comments"

meURLS :: Txt.Text
meURLS = apiMeURLS

meConnectionsURLS :: Txt.Text
meConnectionsURLS = Txt.append apiMeURLS "/connections"

meActivitiesURLS :: Txt.Text
meActivitiesURLS = Txt.append apiMeURLS "/activities"

appsURLS :: Txt.Text
appsURLS = Txt.append apiURL "/apps"

resolveURL :: Txt.Text
resolveURL = Txt.append apiURL "/resolve"

scDownload :: Txt.Text -> IO (Maybe StrLazy.ByteString)
scDownload url =
    do res <- simpleHTTP $ getRequest $ Txt.unpack url
       case res of
         Left _ -> return Nothing
         Right r ->
             case rspCode r of
               (2,_,_) -> return $ Just $ StrLazy.pack $ rspBody r
               (3,_,_) ->
                   case findHeader HdrLocation r of
                     Nothing -> return Nothing
                     Just uri -> scDownload (Txt.pack uri)
               _ -> return Nothing

getJSONText :: Txt.Text -> IO (StrLazy.ByteString)
getJSONText url = 
    do result <- scDownload url
       case result of
         Nothing    -> return "{}"
         Just     r -> return r


scResolve :: [Char] -> IO ()
scResolve url =
    do dat <- getJSONText resolveUrl
       let o = decode dat :: Maybe ScTrack
       case o of
         Nothing  -> StrLazy.putStrLn "Couldn't get anything from response"
         Just obj ->
             case downloadable obj of
               True ->
                   do let dUrlStr = Txt.concat [download_url obj, "?client_id=", clientId]
                      let filename = "./" ++ Txt.unpack (title obj) ++ "." ++ Txt.unpack (original_format obj)
                      StrLazy.putStrLn $ StrLazy.pack ("Fetching to: " ++ filename)
                      contents <- scDownload dUrlStr
                      case contents of
                        Nothing -> StrLazy.putStrLn "Could not fetch file contents"
                        Just  c ->
                            do file <- openBinaryFile filename WriteMode
                               hPutStr file (StrLazy.unpack c)
                               hClose file
               False -> StrLazy.putStrLn "Track is not downloadable"
    where
        txtUrl = Txt.pack url
        resolveUrl = Txt.concat [resolveURL, ".json?url=", txtUrl, "&client_id=", clientId]

