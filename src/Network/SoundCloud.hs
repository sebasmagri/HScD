{- |
   Module:      Network.SoundCloud
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   The 'Network.SoundCloud' module provides functions and types to access
   the soundcloud.com public API.
-}

module Network.SoundCloud (
  scGet,
  scFetch,
  scResolve,
  scResourceType,
  scResourceShowInfo,
  scShowInfo,
  ) where

import Network.SoundCloud.Util (scGet, scFetch, scResolve, scResourceType)

import qualified Network.SoundCloud.App         as App
import qualified Network.SoundCloud.Group       as Group
import qualified Network.SoundCloud.Set         as Set
import qualified Network.SoundCloud.Track       as Track
import qualified Network.SoundCloud.User        as User

-- | Show information about the resource pointed by the given API URL
scResourceShowInfo :: String -> IO ()
scResourceShowInfo url | scResourceType url == "track"   = Track.showInfo url
                       | scResourceType url == "user"    = User.showInfo url
                       | scResourceType url == "set"     = Set.showInfo url
                       | scResourceType url == "group"   = Group.showInfo url
                       | scResourceType url == "app"     = App.showInfo url
scResourceShowInfo _                                     = putStrLn "Unrecognized resource"

-- | Show information about a resource given its public URL
scShowInfo :: String -> IO ()
scShowInfo url =
    do rUrl <- scResolve url
       scResourceShowInfo rUrl
