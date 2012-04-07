{-
  Implementing the SoundCloud API
-}
module Network.SoundCloud (
  scGet,
  scFetch,
  scResolve,
  scResourceType,
  scShowInfo
  ) where

import Network.SoundCloud.Util (scGet, scFetch, scResolve, scResourceType)

import qualified Network.SoundCloud.App as App
import qualified Network.SoundCloud.Group as Group
import qualified Network.SoundCloud.Set as Set
import qualified Network.SoundCloud.Track as Track
import qualified Network.SoundCloud.User as User


scResourceShowInfo :: String -> IO ()
scResourceShowInfo url | scResourceType url == "track"   = Track.showInfo url
                       | scResourceType url == "user"    = User.showInfo
                       | scResourceType url == "set"     = Set.showInfo
                       | scResourceType url == "group"   = Group.showInfo
                       | scResourceType url == "app"     = App.showInfo url
scResourceShowInfo _                                     = putStrLn "Unrecognized resource"

scShowInfo :: String -> IO ()
scShowInfo url =
    do rUrl <- scResolve url
       scResourceShowInfo rUrl