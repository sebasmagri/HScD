{-# LANGUAGE DeriveGeneric #-}

{- |
   Module:      Network.SoundCloud.App
   Copyright:   (c) 2012 Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   License:     BSD3
   Maintainer:  Sebastián Ramírez Magrí <sebasmagri@gmail.com>
   Stability:   experimental

   Represents SoundCloud applications as found at <http://soundcloud.com/apps>
-}

module Network.SoundCloud.App where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Text.Printf (printf)

import Network.SoundCloud.Util (scGet, scResolve)

-- | JSON representation of applications
data JSON = JSON { id                     :: Int
                 , permalink_url          :: String
                 , external_url           :: String
                 , creator                :: String
                 } deriving (Show, Generic)

instance FromJSON JSON
instance ToJSON   JSON

-- | Decode a valid JSON string into an application
-- 'JSON' record
decodeJSON :: String -> Maybe JSON
decodeJSON dat = decode (BSL.pack dat) :: Maybe JSON


-- | Get an application 'JSON' record given a public app URL
-- as in <http://soundcloud.com/apps/app_name>
getJSON :: String -> IO (Maybe JSON)
getJSON url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJSON d

-- | Show general information about an application in the
-- standard output
showInfo :: String -> IO ()
showInfo url =
    do obj <- getJSON url
       case obj of
         Nothing -> putStrLn "Unable to get app information"
         Just o  ->
             do let tmp = "%s\n\t%s by %s\n"
                printf
                  tmp
                  (permalink_url o)
                  (external_url o)
                  (creator o)
