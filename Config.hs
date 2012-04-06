-- | This module takes care of parsing the command-line arguments
-- and defines a few constants
module Config (parseArgs) where

import Control.Monad(unless)
import Data.Maybe(maybe)
import System.Console.GetOpt
import System.Exit
import System.IO

version = "0.1"

versionText = "btrsync, version: " ++ version
helpText = usageInfo "Usage: btrsync [OPTION...] dir1 dir2" options

data Flag = 
      Help 
    | Version
    | Seed Int

options :: [OptDescr Flag]
options =
    [ Option "h" ["help"] (NoArg Help) 
        "Cause the program to print some usage message and exit"
    , Option "v" ["version"] (NoArg Version)
        "Cause the program to print its version number and exit"
    , Option "s" ["seed"] (ReqArg (Seed . read) "NUMBER")
        "Makes the program deterministic by fixing the random number generator's seed"
    ]

data Config = Config {
      seed :: Maybe Int
    }

defaultConfig :: Config
defaultConfig = Config {
      seed = Nothing
    }

actionFromFlag :: Flag -> (Config -> IO Config)
actionFromFlag f c =
    case f of
        Help -> putStrLn helpText >> exitSuccess
        Version -> putStrLn versionText >> exitSuccess
        Seed newSeed -> return c {seed = Just newSeed}

-- | From a list of arguments, return a configuration,
-- the origin directory and the target directory.
-- It is in IO, because it can print version/help messages, and exit the program.
parseArgs :: [String] -> IO (Config, String, String)
parseArgs args = do
    let (flags, nonflags, errs) = getOpt Permute options args
    config <- foldl (>>=) (return defaultConfig) $ map actionFromFlag flags
    unless (null errs) (mapM (hPutStrLn stderr) errs >> exitFailure)
    case nonflags of
        [dir1, dir2] -> return (config, dir1, dir2)
        _ -> do
            putStrLn "Exactly two directories must be provided"
            putStrLn helpText
            exitFailure
    
