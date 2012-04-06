module Main where

import System.Exit
import System.IO
import System.Console.GetOpt
import System.Environment

import Network.SoundCloud

data Options = Options { optTrackURL    :: Maybe String
                       , optResolve     :: Maybe String
                       , optOutput      :: Maybe String
                       }

options :: [OptDescr (Options -> IO Options)]
options = [
        Option "t" ["track"]
            (ReqArg (\arg opt -> return opt { optTrackURL = return arg })
                "URL")
            "Indicate the Track URL to be downloaded."
      , Option "r" ["resolve"]
            (ReqArg (\arg opt -> return opt { optResolve = return arg })
                "URL")
            "Resolve the SoundCloud's API URL for an arbitrary URL. Supports users, tracks, sets, groups and apps"
      , Option "o" ["output"]
            (ReqArg
                (\arg opt -> return opt { optOutput = return arg })
                "FILE")
            "Output File"
      , Option "h" ["help"]
           (NoArg
               (\_ -> exitHelp))
           "Show usage info"
      ]

defaultOptions :: Options
defaultOptions = Options
    { optTrackURL       = Nothing
    , optResolve        = Nothing
    , optOutput         = Nothing
    }

processOpts :: (Maybe String, Maybe String, Maybe String) -> IO ()
processOpts opts@(a,b,c) =
    if all (==Nothing) [a,b,c]
    then exitErrorHelp "No options supplied"
    else
        case opts of
          (Just a0, Nothing, Just c0)   -> scFetchTrack a0 c0
          (Just a0, Nothing, Nothing)   -> scFetchTrack a0 ""
          (Nothing, Just b0, Nothing)   ->
              do uri <- scResolve b0
                 putStrLn uri
          (_, _, _)                  -> exitErrorHelp ""


main :: IO ()
main = do
     args <- getArgs
     let (actions, _, _) = getOpt RequireOrder options args

     opts <- foldl (>>=) (return defaultOptions) actions

     let Options { optTrackURL   = trackUrl
                 , optResolve    = resolve
                 , optOutput     = output
                 } = opts
     let optsTracker = (trackUrl, resolve, output)

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
