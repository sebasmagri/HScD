module Network.SoundCloud.Util where

import Data.List
import Network.HTTP
import System.IO

import Network.SoundCloud.Const

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

scFetch :: String -> String -> IO ()
scFetch dUrl out =
    do contents <- scGet dUrl True
       case contents of
         Nothing -> putStrLn "Could not fetch file contents."
         Just  c ->
             do file <- openBinaryFile out WriteMode
                hPutStr file c
                hClose file

scResourceType :: String -> String
scResourceType url | tracksURL    `isPrefixOf` url      = "track"
                   | usersURL     `isPrefixOf` url      = "user"
                   | playlistsURL `isPrefixOf` url      = "set"
                   | groupsURL    `isPrefixOf` url      = "group"
                   | commentsURL  `isPrefixOf` url      = "comment"
                   | appsURLS     `isPrefixOf` url      = "app"
                   | otherwise                          = "app"

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

