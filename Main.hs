{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Bits
import qualified Data.Map as M
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random
import Network
import System.IO
import System.Process
import System.Posix.Unistd

import Config
import Hashing
import Maths

nextShiftedPrime :: (RandomGen g, MonadState g m) => Integer -> m Integer
nextShiftedPrime = nextPrime . (flip shiftL) 16

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
            neilmachine <- case host t1 of
                Nothing -> do
                    systemid <- getSystemID
                    return $ machine systemid
                Just neil -> return neil
            neil <- case host t1 of
                Nothing -> runCommand 
                    ("btrsync " ++ show config{role=Neil}
                        ++ " " ++ show t1 ++ " " ++ show t2)
                Just neilmachine ->
                    case user t1 of
                        Nothing -> runCommand 
                            ("ssh " ++ neilmachine 
                                ++ " " ++ show config{role=Neil}
                                ++ " " ++ show t1 ++ " " ++ show t2)
                        Just neiluser -> runCommand 
                            ("ssh " ++ neiluser ++ "@" ++ 
                                neilmachine ++ " " ++ show config{role=Neil}
                                ++ " "++ show t1 ++ " " ++ show t2)
            osc <- case host t2 of
                Nothing -> runCommand
                    ("btrsync " ++ show config{role=Oscar}
                        ++ " " ++ show t1{host=Just neilmachine} ++ " " ++ show t2)
                Just oscarmachine ->
                    case user t2 of
                        Nothing -> runCommand                        
                            ("ssh " ++ oscarmachine 
                                ++ " btrsync " ++ show config{role=Oscar}
                                ++ " " ++ show t1{host=Just neilmachine} ++ " " ++ show t2)
                        Just oscaruser -> runCommand 
                            ("ssh " ++ oscaruser ++ "@" ++ 
                                oscarmachine ++ " btrsync " ++ show config{role=Oscar}
                                ++ " " ++ show t1{host=Just neilmachine} ++ " " ++ show t2)
            resultneil <- waitForProcess neil
            resultosc <- waitForProcess osc
            if resultneil /= ExitSuccess then
                error "Neil encountered an error"
                else return ()
            if resultosc /= ExitSuccess then
                error "Oscar encountered an error"
                else return ()

        Oscar -> withSocketsDo $ do
            let g = mkStdGen (seed config)
            oscarg <- getStdGen
            let maybeneil = host t1
                hostname = case maybeneil of
                    Nothing -> error "no hostname for neil."
                    Just neil -> neil
            (_, files2, _) <- toDir (dir t2) ""
            filesPrime2 <- (flip evalStateT) oscarg $ 
                mapKeysM nextShiftedPrime files2
            channel <- connectTo hostname portId
            let ks2 = M.keys filesPrime2 
            hSetBuffering channel LineBuffering        
            let dowhile gen = do
                let (r,g') = randomR (low, high) gen
                    p = (flip evalState) oscarg $ nextPrime r
                    pi2 = mkProduct p ks2
                hPutStrLn channel $ show pi2
                neilData <- hGetLine channel
                if neilData == ""
                    then dowhile g'
                    else 
                        let (b,newFiles) = read neilData :: (Hash,[File]) in
                        oscarTerminate b newFiles filesPrime2
            dowhile g
    
        Neil -> withSocketsDo $ do
            let g = mkStdGen (seed config)
            socket <- listenOn portId
            (channel, _, _) <- accept socket
            nielg <- getStdGen 
            (_, files1, _) <- toDir (dir t1) ""
            filesPrime1 <- (flip evalStateT) nielg $ 
                mapKeysM nextShiftedPrime files1
            let ks1 = M.keys filesPrime1
            hSetBuffering channel LineBuffering
            let dowhile oldD oldPs gen = do
                let (r,g') = randomR (low, high) gen
                    p = (flip evalState) nielg $ nextPrime r
                oscarData <- hGetLine channel
                let pi2 = read oscarData
                    (newPs, d, x) = roundN oldD oldPs p pi2 ks1
                case x of
                    Nothing -> do 
                        hPutStrLn channel ""
                        dowhile d newPs g'
                    Just (b, newHashes) -> 
                        let newFiles = map ((flip M.lookup) filesPrime1) newHashes in
                        hPutStrLn channel (show (b, newFiles))
            dowhile 0 1 g

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
-- All that remains to do are the actual mv/cp/rm/rsync
oscarTerminate :: Integer -> [File] -> M.Map Hash File -> IO ()
oscarTerminate b newFiles filesPrime2 =
    -- filesPrime2 is indexed by the hashes of contents|permissions|path
    -- We first need to get a variant that is indexed by only the contents
    -- in order to find files that were just moved/copied. 
    let filesByContent = 
            M.foldl' (\fbc f@(File p rp fm hc hall) -> M.insert hc f fbc) 
            M.empty filesPrime2
        deleteHashes = detChanges b $ M.keys filesPrime2
        deleteFiles = map ((flip M.lookup) filesPrime2) deleteHashes
    in undefined
