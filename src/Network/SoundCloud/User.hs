{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.User where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Text.Printf (printf)

import Network.SoundCloud.Util (scGet, scResolve)


data JSON = JSON { id                     :: Int
                 , uri                    :: String
                 , permalink_url          :: String
                 , country                :: String
                 , full_name              :: String
                 , city                   :: String
                 , description            :: String
                 , website                :: String
                 , website_title          :: String
                 , online                 :: Bool
                 , track_count            :: Int
                 , playlist_count         :: Int
                 , followers_count        :: Int
                 , followings_count       :: Int
                 , public_favorites_count :: Int
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON

getJSON :: String -> IO (Maybe JSON)
getJSON url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJSON d

showInfo :: String -> IO ()
showInfo url =
    do obj <- getJSON url
       case obj of
         Nothing        -> putStrLn "Unable to get track information."
         Just o         ->
             do let tmp = "%s\n%s (%s)\n\t%s\n%s, %s\n%d tracks, %d sets.\nFollowing: %d\nFollowed by: %d\n"
                printf
                  tmp
                  (permalink_url o)
                  (full_name o)
                  (website o)
                  (description o)
                  (city o)
                  (country o)
                  (track_count o)
                  (playlist_count o)
                  (followings_count o)
                  (followers_count o)

