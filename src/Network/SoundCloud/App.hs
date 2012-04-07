{-# LANGUAGE DeriveGeneric #-}

module Network.SoundCloud.App where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Text.Printf (printf)

import Network.SoundCloud.Util (scGet, scResolve)

data Json = Json { id                     :: Int
                 , permalink_url          :: String
                 , external_url           :: String
                 , creator                :: String
                 } deriving (Show, Generic)

instance FromJSON Json
instance ToJSON   Json

decodeJson :: String -> Maybe Json
decodeJson dat = decode (BSL.pack dat) :: Maybe Json

getJson :: String -> IO (Maybe Json)
getJson url =
    do tUrl <- scResolve url
       dat  <- scGet tUrl True
       case dat of
         Nothing -> return Nothing
         Just d  -> return $ decodeJson d

showInfo :: String -> IO ()
showInfo url =
    do obj <- getJson url
       case obj of
         Nothing -> putStrLn "Unable to get app information"
         Just o  ->
             do let tmp = "%s\n\t%s by %s\n"
                printf
                  tmp
                  (permalink_url o)
                  (external_url o)
                  (creator o)
