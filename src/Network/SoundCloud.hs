{- |
   Module:      Network.SoundCloud
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   The @Network.SoundCloud@ module provides functions and types to access
   the soundcloud.com public API.

   /Resource Modules/

   The library is composed by a set of modules implementing datatypes and
   functions for the resources available through the API. Namely,

     * "Network.SoundCloud.App"

     * "Network.SoundCloud.Comment"

     * "Network.SoundCloud.Group"

     * "Network.SoundCloud.MiniUser"

     * "Network.SoundCloud.Set"

     * "Network.SoundCloud.Track"

     * "Network.SoundCloud.User"

   Every resource module defines at least a @JSON@ record and the @getJSON@ and @decodeJSON@ functions.

   /Base Modules/

   The base modules provide values and functions that are to be used by the rest of the modules in the
   library. The base modules are,

     * "Network.SoundCloud.Const"

     * "Network.SoundCloud.Util"

   /API Documentation/

   Documentation of the SoundCloud's API can be found at <http://developers.soundcloud.com/docs/>
-}

module Network.SoundCloud (
  scSimpleGet,
  scRecursiveGet,
  scFetch,
  scResolve,
  scResourceType,
  scResourceShowInfo,
  scShowInfo
  ) where

import Network.SoundCloud.Util (scSimpleGet, scRecursiveGet, scFetch, scResolve, scResourceType)

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
scShowInfo url = scResourceShowInfo =<< scResolve url
