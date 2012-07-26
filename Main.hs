{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Concurrent
import Control.Exception as C
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Bits
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Network
import Network.BSD
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Unistd
import System.Process
import System.Random

import Config
import Hashing
import Maths

tryWhile :: IO a -> IO a
tryWhile action = do
    lr <- C.try action
    case lr of
        Right x -> return x
        Left (_ :: IOException) -> tryWhile action

waitSome :: Handle -> IO ()
waitSome channel = do
    eof <- hIsEOF channel
    when eof $ waitSome channel

nextShiftedPrime :: (RandomGen g) => Integer -> StateT g IO Integer
nextShiftedPrime i = 
    lift (debug ("???: nextPrime " ++ show i)) >>
    nextPrime (shiftL i 16)

mapKeysM :: (Monad m, Ord key, Ord key') => 
    (key -> m key') -> M.Map key val -> m (M.Map key' val)
mapKeysM f =
    liftM M.fromList . mapM (\(k, v) -> do
            k' <- f k 
            return (k', v)
        ) . M.toList

main :: IO ()
main = do 
    args <- getArgs
    (config, t1, t2) <- parseArgs args        
    let low = 2 ^ (pSize config) :: Integer
        high = 2 ^ (pSize config + 1) :: Integer
        portId = port config
    case role config of
        Main -> do
            neilMachine <- case host t1 of
                Nothing -> do
                    hostName <- getHostName
                    return hostName
                Just neil -> return neil
            let btrsyncCommandNeil = "btrsync" ++ show config{role=Neil} ++ " "
                    ++ show t1 ++ " " ++ show t2
                commandNeil = case host t1 of
                    Nothing -> btrsyncCommandNeil
                    Just neilMachine ->
                        case user t1 of
                            Nothing -> 
                                "ssh " ++ neilMachine ++ " " ++ show btrsyncCommandNeil
                            Just neilUser -> 
                                "ssh " ++ neilUser ++ "@" ++ neilMachine 
                                ++ " " ++ show btrsyncCommandNeil
            debug ("MAIN: commandNeil: " ++ commandNeil)
            neil <- runCommand commandNeil
            let btrsyncCommandOscar = "btrsync" ++ show config{role=Oscar} ++ " "
                    ++ show t1{host=Just neilMachine} ++ " " ++ show t2 
                commandOscar = case host t2 of
                    Nothing -> btrsyncCommandOscar
                    Just oscarMachine ->
                        case user t2 of
                            Nothing ->
                                "ssh " ++ oscarMachine ++ " " ++ show btrsyncCommandOscar
                            Just oscarUser ->
                                "ssh " ++ oscarUser ++ "@" ++ oscarMachine 
                                ++ " " ++ show btrsyncCommandOscar
            debug ("MAIN: commandOscar: " ++ commandOscar)
            osc <- runCommand commandOscar
            _ <- installHandler sigKILL (Catch (terminateProcess neil >> terminateProcess osc)) 
                (Just (addSignal sigQUIT (addSignal sigINT emptySignalSet)))
            resultNeil <- waitForProcess neil
            resultOsc <- waitForProcess osc
            unless (resultNeil == ExitSuccess) $
                error "Neil encountered an error"
            unless (resultOsc == ExitSuccess) $
                error "Oscar encountered an error"
            exitSuccess

        Oscar -> withSocketsDo $ do
            let g = mkStdGen (seed config)
            oscarg <- getStdGen
            let hostname = case host t1 of
                    Nothing -> error "no hostname for neil."
                    Just neil -> neil
            debug "OSCAR: before connectTo"
            channel <- tryWhile (connectTo hostname portId) 
            debug "OSCAR: before setting current directory"
            setCurrentDirectory $ dir t2
            (files2, dirs2) <- crawlDir (dir t2) ""
            filesPrime2 <- (flip evalStateT) oscarg $ 
                mapKeysM nextShiftedPrime files2
            dirsPrime2 <- (flip evalStateT) oscarg $
                mapKeysM nextShiftedPrime dirs2
            let ks2 = M.keys filesPrime2 ++ M.keys dirsPrime2 
            hSetBuffering channel LineBuffering
            debug "OSCAR: before dowhile"       
            let dowhile gen = do
                let (r,g') = randomR (low, high) gen
                    p = (flip evalState) oscarg $ nextPrime r
                    pi2 = mkProduct p ks2
                hPutStrLn channel $ show pi2
                waitSome channel
                neilData <- hGetLine channel
                debug ("OSCAR: received from Neil: " ++ neilData)
                if neilData == ""
                    then dowhile g'
                    else 
                        let (b, newDirs, newFiles) = read neilData :: (Hash,[Dir], [File]) in
                        oscarTerminate b newFiles newDirs filesPrime2 dirsPrime2 
                            ks2 hostname (user t1) (dir t2)
            dowhile g
            exitSuccess
    
        Neil -> withSocketsDo $ do
            let g = mkStdGen (seed config)
            debug "NEIL: before listenOn"
            socket <- listenOn portId
            debug "NEIL: between listenOn and accept"
            (channel, _, _) <- accept socket
            debug "NEIL: after accept"
            nielg <- getStdGen
            (files1, dirs1) <- crawlDir (dir t1) ""
            filesPrime1 <- (flip evalStateT) nielg $ 
                mapKeysM nextShiftedPrime files1
            dirsPrime1 <- (flip evalStateT) nielg $
                mapKeysM nextShiftedPrime dirs1
            let ks1 = M.keys filesPrime1 ++ M.keys dirsPrime1
            hSetBuffering channel LineBuffering
            let dowhile oldD oldPs gen = do
                let (r,g') = randomR (low, high) gen
                    p = (flip evalState) nielg $ nextPrime r
                waitSome channel
                oscarData <- hGetLine channel
                let pi2 = read oscarData
                    (newPs, d, x) = roundN oldD oldPs p pi2 ks1
                case x of
                    Nothing -> do 
                        hPutStrLn channel ""
                        dowhile d newPs g'
                    Just (b, newHashes) -> 
                        let newFiles = catMaybes $ map ((flip M.lookup) filesPrime1) newHashes
                            newDirs = catMaybes $ map ((flip M.lookup) dirsPrime1) newHashes
                        in hPutStrLn channel (show (b, newDirs, newFiles))
            dowhile 0 1 g
            sClose socket
            exitSuccess 

-- | This contains all the computations on Neil side for a round
roundN :: Integer -> Integer -> Integer -> Integer -> [Hash]
        -> (Integer, Integer, Maybe (Integer, [Hash]))
roundN oldD oldPs p pi2 ks1 =
    let newPs = p * oldPs
        pi1 = mkProduct p ks1
        d' = (pi1 * modularInv p pi2) `mod` p
        -- TODO: we hope that p is not a factor of modulo
        d = d' * oldPs * modularInv p oldPs + oldD * p * modularInv oldPs p
        (a, b) = minFraction d newPs
        newHashes = detChanges a ks1
        okNew = (product newHashes `mod` newPs) == a
    in
    if okNew
        then (newPs, d, Just (b, newHashes))
        else (newPs, d, Nothing)

-- | When this function is called, all necessary information has been exchanged
-- All that remains to do are the actual mkdir/cp/rm/rsync/..
oscarTerminate :: Integer -> [File] -> [Dir] -> M.Map Hash File -> M.Map Hash Dir
    -> [Hash] -> String -> Maybe String -> FilePath -> IO ()
oscarTerminate b newFiles newDirs filesPrime2 dirsPrime2 ks2 hostNeil userNeil dirOscar =
    -- filesPrime2 is indexed by the hashes of contents|permissions|path
    -- We first need to get a variant that is indexed by only the contents
    -- in order to find files that were just moved/copied. 
    let filesByContent = 
            M.foldl' (\fbc f@(File p rp fm hc hall) -> M.insert hc f fbc) 
            M.empty filesPrime2
        deleteHashes = detChanges b ks2
        deleteFiles = catMaybes $ map ((flip M.lookup) filesPrime2) deleteHashes
        deleteDirs = catMaybes $ map ((flip M.lookup) dirsPrime2) deleteHashes
    in do
    -- TODO: add new directories
    forM_ newDirs (\ d -> do
            let instruction = "mkdir -p " ++ combine dirOscar (dRelativePath d)
            debug ("OSCAR: " ++ instruction)
            system instruction
        )

    -- TODO: fix new directory permissions

    -- If the new files are in fact old ones
    reallyNewFiles <- filterM (\ f@(File _ rp fm hc _) -> case M.lookup hc filesByContent of
        Nothing -> return True
        Just fOld@(File _ oldRp oldFm _ _) -> do
            let instruction = "cp " ++ combine dirOscar oldRp ++ " " ++ combine dirOscar rp
            debug ("OSCAR: " ++ instruction)
            errorCode <- system instruction 
            case errorCode of
                ExitSuccess -> return False
                ExitFailure _ -> error (show instruction ++ " failed")
        ) newFiles
    -- Then we find which files were really deleted and not just modified
    -- These we delete by calling rm
    forM_ deleteFiles (\ f@(File _ rp fm hc _) -> 
        case find (\newF -> fHashContent newF == hc) reallyNewFiles of
            Nothing -> do
                let instruction = "rm " ++ combine dirOscar rp
                debug ("OSCAR: " ++ instruction)
                errorCode <- system instruction
                case errorCode of
                    ExitSuccess -> return ()
                    ExitFailure _ -> error (show instruction ++ " failed")
        )
    -- Then, we delegate to rsync for synchronising files
    let neilOrigin = maybe 
            (hostNeil ++ ":") 
            (\u -> u ++ "@" ++ hostNeil ++ ":") 
            userNeil
    forM_ reallyNewFiles (\ f@(File p rp _ _ _) -> do
        let instruction = "rsync " ++ neilOrigin ++ p ++ " " ++ combine dirOscar rp
        debug ("OSCAR: " ++ instruction)
        errorCode <- system instruction
        case errorCode of
            ExitSuccess -> return ()
            ExitFailure _ -> error "rsync failed"
        )
    -- Finally we fix permissions
    forM_ newFiles (\ f@(File _ rp fm _ _) -> do
            debug ("OSCAR: setFileMode " ++ combine dirOscar rp ++ " " ++ show fm)
            setFileMode rp fm
        )

    -- TODO: then we delete old directories
    forM_ deleteDirs (\ d -> do
            let instruction = "rm -rf " ++ combine dirOscar (dRelativePath d)
            debug ("OSCAR: " ++ instruction)
            system instruction
        ) 
