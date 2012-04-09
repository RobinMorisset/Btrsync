-- | This module produces the tree/set of directories and files, along with their hash,
-- from a FilePath
module Hashing (Dir(..), File(..), Hash(..), toDir) where

import Prelude hiding (readFile)
import Control.Monad
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import Data.List
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix

type Hash = Integer
data File = File FilePath Hash
    deriving (Eq, Show)
data Dir = Dir FilePath Hash [File] [Dir]
    deriving (Eq, Show)

subDirs (Dir _ _ ds _) = ds
subFiles (Dir _ _ _ fs) = fs

hashFile :: FilePath -> IO Integer
hashFile path = B.readFile path >>= (return . integerDigest . sha1)

toFile :: FilePath -> IO File
toFile path = do
    h <- hashFile path
    return $ File path h

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
    let hashes = map (\ (File _ h) -> h) files 
            ++ map (\ (Dir _ h _ _) -> h) dirs in
    foldl xor 0 hashes -- TODO: replace by a true hash

-- | This function returns both the 'Dir' tree, along
-- with a set of all file hashes beneath it (recursively)
-- and a set of all directory hashes beneath it (recursively, including itself)
toDir :: FilePath -> IO (Dir, M.Map Integer File, M.Map Integer Dir)
toDir path = do
    contents <- getDirectoryContents path
    -- Because getDirectoryContents return file/directory names and not paths
    let contents_absolute = map (combine path) contents
    validPaths <- mapM getPathValidity contents_absolute
    let (filesP, dirsP) = Prelude.foldl (\ (fs, ds) p -> case p of
            PVFile filepath -> (filepath : fs, ds)
            PVDir dirpath -> (fs, dirpath : ds)
            PVFail -> (fs, ds)
            ) ([], []) validPaths
    files <- mapM toFile filesP
    (dirs, fileHashes, dirHashes) <- addDirs ([], M.empty, M.empty) dirsP
    let h = hashDir files dirs
        d = Dir path h files dirs
        fhs = Prelude.foldl (\ m f@(File _ h) -> M.insert h f m) fileHashes files
        dhs = M.insert h d dirHashes 
    return (d, fhs, dhs)
    where
        addDirs :: ([Dir], M.Map Integer File, M.Map Integer Dir) -> [FilePath]
                -> IO ([Dir], M.Map Integer File, M.Map Integer Dir)
        addDirs acc [] = return acc
        addDirs (ds, fhs, dhs) (dPath : dPaths) = do
            (d, fhs', dhs') <- toDir dPath
            addDirs (d : ds, M.union fhs fhs', M.union dhs dhs') dPaths
