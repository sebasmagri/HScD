{-# OPTIONS_HADDOCK hide #-}

module Main where

import System.Exit
import System.IO
import System.Console.GetOpt
import System.Environment

import Network.SoundCloud (scResolve, scShowInfo)
import qualified Network.SoundCloud.Track as Track

data Options = Options { optTrackURL    :: Maybe String
                       , optOutput      :: Maybe String
                       , optInfo        :: Maybe String
                       , optResolve     :: Maybe String
                       }

options :: [OptDescr (Options -> IO Options)]
options = [
        Option "t" ["track"]
            (ReqArg (\arg opt -> return opt { optTrackURL = return arg })
                "URL")
            "Indicate the URL of the track to be downloaded."
      , Option "o" ["output"]
            (ReqArg
                (\arg opt -> return opt { optOutput = return arg })
                "FILE")
            "Output File"
      , Option "i" ["info"]
            (ReqArg
                (\arg opt -> return opt { optInfo = return arg })
                "URL")
            "Get info about the resource pointed by the URL"
      , Option "r" ["resolve"]
            (ReqArg (\arg opt -> return opt { optResolve = return arg })
                "URL")
            "Resolve the SoundCloud's API URL for an arbitrary URL. Supports users, tracks, sets, groups and apps"
      , Option "h" ["help"]
            (NoArg
                (\_ -> exitHelp))
            "Show usage info"
      ]

defaultOptions :: Options
defaultOptions = Options
    { optTrackURL       = Nothing
    , optOutput         = Nothing
    , optInfo           = Nothing
    , optResolve        = Nothing
    }

processOpts :: (Maybe String, Maybe String, Maybe String, Maybe String) -> IO ()
processOpts opts =
    case opts of
      (Just a0, Just b0, Nothing, Nothing)   -> Track.fetch a0 b0
      (Just a0, Nothing, Nothing, Nothing)   -> Track.fetch a0 ""
      (Nothing, Nothing, Just c0, Nothing)   -> scShowInfo c0
      (Nothing, Nothing, Nothing, Just d0)   ->
          do uri <- scResolve d0
             putStrLn uri
      (_, _, _, _)                  -> exitErrorHelp ""


main :: IO ()
main = do
     args <- getArgs
     let (actions, _, _) = getOpt RequireOrder options args

     opts <- foldl (>>=) (return defaultOptions) actions

     let Options { optTrackURL   = trackUrl
                 , optOutput     = output
                 , optInfo       = info
                 , optResolve    = resolve
                 } = opts
     let optsTracker = (trackUrl, output, info, resolve)

     processOpts optsTracker

exitErrorHelp :: String -> IO a
exitErrorHelp msg = do
    hPutStrLn stderr msg
    hPutStrLn stderr ""
    showHelp
    exitFailure

showHelp :: IO ()
showHelp = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    hFlush stderr

exitHelp :: IO a
exitHelp = do
    showHelp
    exitSuccess
