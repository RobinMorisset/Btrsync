-- | This module produces the tree/set of directories and files, along with their hash,
-- from a FilePath
module Hashing (Dir(..), File(..), Hash(..), toDir) where

import Prelude hiding (readFile)
import Control.Monad
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as Bi
import Data.Digest.Pure.SHA
import Data.List
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import System.Posix.Types

type Hash = Integer
-- | The first hash only hashes the contents of the file, while the second one
--  also hashes its path and permissions
data File = File FilePath FilePath FileMode Hash Hash
    deriving (Eq, Show, Read)
data Dir = Dir FilePath Hash [File] [Dir]
    deriving (Eq, Show, Read)

subDirs (Dir _ _ ds _) = ds
subFiles (Dir _ _ _ fs) = fs

-- | Takes a path to a file and its relative path, and produces the 
-- corresponding file datastructure
toFile :: FilePath -> String -> IO File
toFile path relPath = do
    f <- B.readFile path
    fStatus <- getFileStatus path
    let fMode = fileMode fStatus
        toBeHashed = B.append (B.pack $ map Bi.c2w (show fMode ++ relPath)) f
        h1 = integerDigest $ sha1 f
        h2 = integerDigest $ sha1 toBeHashed
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

hashDir :: [File] -> [Dir] -> Hash
hashDir files dirs =
    let hashes = map (\ (File _ _ _ _ h) -> h) files 
            ++ map (\ (Dir _ h _ _) -> h) dirs in
    foldl xor 0 hashes -- TODO: replace by a true hash

firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM f (x, y) = do
    fx <- f x
    return (fx, y)

-- | This function returns both the 'Dir' tree, along
-- with a set of all file hashes beneath it (recursively)
-- and a set of all directory hashes beneath it (recursively, including itself)
toDir :: FilePath -> FilePath -> IO (Dir, M.Map Integer File, M.Map Integer Dir)
toDir path relPath = do
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
    (dirs, fileHashes, dirHashes) <- addDirs ([], M.empty, M.empty) dirsP
    let h = hashDir files dirs
        d = Dir path h files dirs
        fhs = Prelude.foldl (\ m f@(File _ _ _ _ h) -> M.insert h f m) fileHashes files
        dhs = M.insert h d dirHashes 
    return (d, fhs, dhs)
    where
        addDirs :: ([Dir], M.Map Integer File, M.Map Integer Dir) -> [(FilePath, FilePath)]
                -> IO ([Dir], M.Map Integer File, M.Map Integer Dir)
        addDirs acc [] = return acc
        addDirs (ds, fhs, dhs) ((dPath, dRelPath) : dPaths) = do
            (d, fhs', dhs') <- toDir dPath dRelPath
            addDirs (d : ds, M.union fhs fhs', M.union dhs dhs') dPaths
