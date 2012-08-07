-- | This module produces the tree/set of directories and files, along with their hash,
-- from a FilePath
module Hashing (Dir(..), File(..), Hash(..), crawlDir, debug) where

import Prelude hiding (readFile)
import Control.Monad
import Control.Exception
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as Bi
import Data.Digest.Pure.SHA
import Data.List
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Types
import System.Process

debug :: String -> IO ()
debug s = do
    hPutStrLn stderr s
--     _ <- system ("echo " ++ show s ++ " >> ~/btrsync.log")
-- For showing all the open file descriptors
--     _ <- system "lsof -c btrsync -d \"^mem\" -a >> ~/btrsync.lsof"
    return ()

type Hash = Integer
-- | The first hash only hashes the contents of the file, while the second one
--  also hashes its path and permissions
data File = File
    { fAbsolutePath :: FilePath 
    , fRelativePath :: FilePath 
    , fFileMode :: FileMode 
    , fHashContent :: Hash
    , fHashAll :: Hash
    }
    deriving (Eq, Show, Read)

data Dir = Dir
    { dAbsolutePath :: FilePath
    , dRelativePath :: FilePath
    , dFileMode :: FileMode
    , dHashRelPath :: Hash
    }
    deriving (Eq, Show, Read)

-- | Takes a path to a file and its relative path, and produces the 
-- corresponding file datastructure
toFile :: FilePath -> String -> IO File
toFile path relPath = do
    f <- B.readFile path
    fStatus <- getFileStatus path
    let fMode = fileMode fStatus
        toBeHashed = B.append (B.pack $ map Bi.c2w (show fMode ++ relPath)) f
    h1 <- evaluate $ integerDigest $ sha1 f
    h2 <- evaluate $ integerDigest $ sha1 toBeHashed
    return $ File path relPath fMode h1 h2

data PathValidity = PVFile FilePath | PVDir FilePath | PVFail
    deriving (Show, Eq)

getPathValidity :: FilePath -> IO PathValidity
getPathValidity path = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    return $ case (isFile, isDir) of
        (True, False) -> PVFile path
        (False, True) | not (isSuffixOf "." path) 
                    && not (isSuffixOf ".." path) -> PVDir path
        _ -> PVFail 

firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM f (x, y) = do
    fx <- f x
    return (fx, y)

toDir :: FilePath -> FilePath -> IO Dir
toDir p rp = do
    dStatus <- getFileStatus p
    let dMode = fileMode dStatus
        h = integerDigest . sha1 . B.pack $ map Bi.c2w (show dMode ++ rp)
    return $ Dir p rp dMode h

crawlDir :: FilePath -> FilePath -> IO (M.Map Integer File, M.Map Integer Dir)
crawlDir path relPath = do
    debug ("???: crawldir " ++ path)
    contents <- getDirectoryContents path
    -- Because getDirectoryContents return file/directory names and not paths
    let contents_path = map (\n -> (combine path n, combine relPath n)) contents
    validPaths <- mapM (firstM getPathValidity) contents_path
    let (filesP, dirsP) = Prelude.foldl (\ (fs, ds) p -> case p of
            (PVFile filepath, rp) -> ((filepath, rp) : fs, ds)
            (PVDir dirpath, rp) -> (fs, (dirpath, rp) : ds)
            (PVFail, _) -> (fs, ds)
            ) ([], []) validPaths
    files <- mapM (\(p, rp) -> toFile p rp) filesP
    (dirs, rFilesAndDirs) <- liftM unzip $ mapM (\(p, rp) -> do
            d <- toDir p rp
            rFD <- crawlDir p rp
            return (d, rFD)
        ) dirsP
    let filesMap = foldl (\ m f -> M.insert (fHashAll f) f m) M.empty files
        dirsMap = foldl (\ m d -> M.insert (dHashRelPath d) d m) M.empty dirs
        (rFiles, rDirs) = foldl (\ (fs, ds) (rfs, rds) -> 
                (M.union fs rfs, M.union ds rds)
            ) (filesMap, dirsMap) rFilesAndDirs   
    return (rFiles, rDirs)
