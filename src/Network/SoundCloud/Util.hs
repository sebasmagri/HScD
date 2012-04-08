{- |
   Module:      Network.SoundCloud.Util
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   General functions used by other modules
-}

module Network.SoundCloud.Util (
  scSimpleGet,
  scRecursiveGet,
  scFetch,
  scResourceType,
  scResolve
  ) where

import Data.List
import Network.HTTP
import System.IO

import Network.SoundCloud.Const

-- | Issue a @GET@ request to an URL given as second parameter
-- and returns the response body as a 'String' or 'Nothing'
-- on failure.
--
-- If the first argument is set to 'True', and a @3XX@
-- response code is found, a new request will be made
-- to the @Location@ header of the response.
scGet :: Bool -> String -> IO (Maybe String)
scGet followRedirections url =
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
                         then scRecursiveGet uri
                         else return $ Just uri
               _ -> return Nothing

-- | Issue a @GET@ HTTP request to the passed URL returning
-- @Nothing@ if a response code different than 2XX is found.
scSimpleGet :: String -> IO (Maybe String)
scSimpleGet = scGet False

-- | Issue a @GET@ HTTP request to the passed URL recursing
-- over redirections
scRecursiveGet :: String -> IO (Maybe String)
scRecursiveGet = scGet True

-- | Given an URL as a first parameter, and a path as a second,
-- issue a @GET@ request to the @URL@ and save the response body
-- to a file at @path@.
scFetch :: String -> String -> IO ()
scFetch dUrl out =
    do contents <- scRecursiveGet dUrl
       case contents of
         Nothing -> putStrLn "Could not fetch file contents."
         Just  c ->
             do file <- openBinaryFile out WriteMode
                hPutStr file c
                hClose file

-- | Given an arbitrary resource URL, returns the type of the
-- resource.
--
-- The response can be one of:
--
--   * @track@
--
--   * @user@
--
--   * @set@
--
--   * @group@
--
--   * @comment@
--
--   * @app@
--
--   * @nothing@
scResourceType :: String -> String
scResourceType url | tracksURL    `isPrefixOf` url      = "track"
                   | usersURL     `isPrefixOf` url      = "user"
                   | playlistsURL `isPrefixOf` url      = "set"
                   | groupsURL    `isPrefixOf` url      = "group"
                   | commentsURL  `isPrefixOf` url      = "comment"
                   | appsURLS     `isPrefixOf` url      = "app"
                   | otherwise                          = "nothing"

{-
This function's request will always return a (3,_,_) status,
so we can just return the redirection Location
-}
-- | Get the API url of a resource given its public URL.
-- In example, for a public URL like:
--
--     <http://soundcloud.com/user/track>
--
-- It returns the API URL:
--
--     <http://api.soundcloud.com/tracks/track_id.json?client_id=foo>
scResolve :: String -> IO String
scResolve url =
    do dat <- scSimpleGet resolveUrl
       case dat of
         Nothing        -> return ""
         Just d         -> return d
    where
        resolveUrl = concat [resolveURL, ".json?url=", url, "&client_id=", clientId]

