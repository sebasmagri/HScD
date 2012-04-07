{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.App where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Text.Printf (printf)

import Network.SoundCloud.Util (scGet, scResolve)

data JSON = JSON { id                     :: Int
                 , permalink_url          :: String
                 , external_url           :: String
                 , creator                :: String
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
         Nothing -> putStrLn "Unable to get app information"
         Just o  ->
             do let tmp = "%s\n\t%s by %s\n"
                printf
                  tmp
                  (permalink_url o)
                  (external_url o)
                  (creator o)
