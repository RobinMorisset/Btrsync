-- | This module takes care of parsing the command-line arguments
-- and defines a few constants
module Config (Config(..), Target(..), parseArgs) where

import Control.Monad(unless)
import Data.Maybe(maybe)
import Data.List
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
    | PSize Int

options :: [OptDescr Flag]
options =
    [ Option "h" ["help"] (NoArg Help) 
        "Cause the program to print some usage message and exit"
    , Option "v" ["version"] (NoArg Version)
        "Cause the program to print its version number and exit"
    , Option "s" ["seed"] (ReqArg (Seed . read) "NUMBER")
        "Makes the program deterministic by fixing the random number generator's seed"
    , Option "p" ["pSize"] (ReqArg (PSize . read) "NUMBER")
        "Changes the size of the prime number used at each round"
    ]

data Config = Config {
      seed :: Maybe Int -- ^ Seed of the random generator
    , pSize :: Int -- ^ Size of p in bits
    }

defaultConfig :: Config
defaultConfig = Config {
      seed = Nothing
    , pSize = 1000
    }

actionFromFlag :: Flag -> (Config -> IO Config)
actionFromFlag f c =
    case f of
        Help -> putStrLn helpText >> exitSuccess
        Version -> putStrLn versionText >> exitSuccess
        Seed newSeed -> return c {seed = Just newSeed}
        PSize size -> return c {pSize = size}

-- | From a list of arguments, return a configuration,
-- the origin directory and the target directory.
-- It is in IO, because it can print version/help messages, and exit the program.
parseArgs :: [String] -> IO (Config, Target, Target)
parseArgs args = do
    let (flags, nonflags, errs) = getOpt Permute options args
    config <- foldl (>>=) (return defaultConfig) $ map actionFromFlag flags
    unless (null errs) (mapM (hPutStrLn stderr) errs >> exitFailure)
    case nonflags of
        [target1, target2] -> return (config, parseTarget target1, parseTarget target2)
        _ -> do
            putStrLn "Exactly two targets must be provided"
            putStrLn helpText
            exitFailure

-- | A target is either the origin or the end-point of the synchronization.
-- If host is 'None' (ie localhost), then user is too.
data Target = Target {
      user :: Maybe String
    , host :: Maybe String
    , dir :: FilePath
    }
    deriving (Show)

parseTarget :: String -> Target
parseTarget str =
    let (beforeCol, afterCol) = break (== ':') str in
    if null afterCol -- No colon has been found
        then Target Nothing Nothing beforeCol
        else let (beforeAro, afterAro) = break (== '@') beforeCol in
            if null afterAro -- No arobase has been found
                then Target Nothing (Just beforeCol) (tail afterCol)
                else Target (Just beforeAro) (Just $ tail afterAro) (tail afterCol)
