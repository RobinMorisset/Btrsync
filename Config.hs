-- | This module takes care of parsing the command-line arguments
-- and defines a few constants
module Config (Config(..), Target(..), Role(..), parseArgs) where

import Control.Monad(unless)
import Data.Maybe(maybe)
import Data.List
import Network
import System.Console.GetOpt
import System.Exit
import System.IO

version = "0.1"

versionText = "btrsync, version: " ++ version
helpText = usageInfo "Usage: btrsync [OPTION...] dir1 dir2" options

data Role = Main | Neil | Oscar

data Flag = 
      Help 
    | Version
    | Seed Int
    | PSize Int
    | Role Role
    | Port PortID

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
    , Option "" ["port"] (ReqArg (Port . PortNumber . fromInteger . read) "NUMBER")
        "Chooses the port to use for communication. Default: 1337"
    , Option "" ["isOrigin"] (NoArg $ Role Neil)
        "INTERNAL USE, for telling btrsync it is Neil in the protocol"
    , Option "" ["isDestination"] (NoArg $ Role Oscar)
        "INTERNAL USE, for telling btrsync it is Oscar in the protocol"
    ]

data Config = Config {
      seed :: Int -- ^ Seed of the random generator
    , pSize :: Int -- ^ Size of p in bits
    , role :: Role -- ^ whether btrsync has been directly called by the user, 
                    -- or is performing one half of the protocol
    , port :: PortID
    }
instance Show Config where
    show c =
        let s = " -s " ++ show (seed c)
            p = " -p " ++ show (pSize c)
            r = case role c of
                    Neil -> " --isOrigin"
                    Oscar -> " --isDestination"
                    Main -> ""
            portId = case port c of
                    PortNumber i -> " --port " ++ show (toInteger i)
                    _ -> error "the port identifier wasn't an integer"
        in
        s ++ p ++ r ++ portId

defaultConfig :: Config
defaultConfig = Config {
      seed = 42
    , pSize = 1000
    , role = Main
    , port = PortNumber 1337
    }

actionFromFlag :: Flag -> (Config -> IO Config)
actionFromFlag f c =
    case f of
        Help -> putStrLn helpText >> exitSuccess
        Version -> putStrLn versionText >> exitSuccess
        Seed newSeed -> return c {seed = newSeed}
        PSize size -> return c {pSize = size}
        Role newRole -> return c {role = newRole}
        Port newPort -> return c {port = newPort}

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
