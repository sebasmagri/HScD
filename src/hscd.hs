module Main where

import System.Exit
import System.IO
import System.Console.GetOpt
import System.Environment

import Network.SoundCloud

data Options = Options { optURL :: IO String
                       , optOutput :: String -> IO ()}
options :: [OptDescr (Options -> IO Options)]
options = [
        Option "u" ["url"]
            (ReqArg (\arg opt -> return opt { optURL = return arg })
                "URL")
            "Track URL"
      , Option "o" ["output"]
            (ReqArg
                (\arg opt -> return opt { optOutput = writeFile arg })
                "OUTPUT")
            "Output File"
      , Option "h" ["help"]
           (NoArg
               (\_ -> exitHelp))
           "Show usage info"
      ]

defaultOptions :: Options
defaultOptions = Options
    { optURL     = exitErrorHelp "use -u to indicate the track's URL"
    , optOutput  = putStr
    }

main :: IO ()
main = do
     args <- getArgs
     let (actions, nonOptions, errors) = getOpt RequireOrder options args

     opts <- foldl (>>=) (return defaultOptions) actions

     let Options { optURL = url
                 , optOutput = output } = opts
     strUrl <- url
     scResolve strUrl


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
    exitWith ExitSuccess
