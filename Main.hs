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

-- | makePrime low high returns p such that
-- p >= low
-- not exists p' s.t. p > p' >= high
makePrime :: Integer -> Integer -> StateT StdGen IO Integer
makePrime low high = do
    g <- get
    let (preP, g') = randomR (low, high) g
    put g'
    nextPrime preP

mainO :: Handle -> Handle -> Handle -> IO ()
mainO input output inputEnd =
    

main :: IO ()
main = 
    case role config of
	Main -> do
            args <- getArgs
            (config, t1, t2) <- parseArgs args
            g <- case seed config of
                    Nothing -> getStdGen
                    Just s -> return $ mkStdGen s
            let low = 2 ^ (pSize config) :: Integer
                high = 2 ^ (pSize config + 1) :: Integer

            (_, files1, _) <- toDir $ dir t1
            (_, files2, _) <- toDir $ dir t2

            (newFiles, deleteFiles) <- (flip evalStateT) g $ do
                filesPrime1 <- mapKeysM nextShiftedPrime files1
                filesPrime2 <- mapKeysM nextShiftedPrime files2
                let ks1 = M.keys filesPrime1
                let ks2 = M.keys filesPrime2 

                let whilenot :: Integer -> Integer -> StateT StdGen IO ([Hash], [Hash])
                    whilenot oldD modulo = do
                    p <- makePrime low high 
                    let newP = p * modulo 

                    -- Currently, we are only considering not-recursive dirs
                    let pi1 = mkProduct p ks1
                    let pi2 = mkProduct p ks2

                    let d' = (pi1 * modularInv p pi2) `mod` p 
                    let d = crt [(d',p), (oldD, modulo)]
                    let (a, b) = minFraction d newP

                    let newHashes = detChanges a ks1
                        deleteHashes = detChanges b ks2
                        okNew = (product newHashes `mod` newP) == a
                        okDelete = (product deleteHashes `mod` newP) == b

                    if (okNew && okDelete) 
                        then return (newHashes, deleteHashes)
                        else whilenot d newP
                
                (newHashes, deleteHashes) <- whilenot 0 1
            
                let newFiles = catMaybes $ 
                        map (\h -> M.lookup h filesPrime1) newHashes
                    deleteFiles = catMaybes $ 
                        map (\h -> M.lookup h filesPrime2) deleteHashes
                return (newFiles, deleteFiles)

            print newFiles
            print deleteFiles
            exitSuccess

    Oscar -> withSocketsDo $ do
        args <- getArgs
        (config, t1, t2) <- parseArgs args
        channel <- connectTo 
        oscarg <- getStdGen
        maybeneil <- host t1
        let hostname = case maybeneil of
                    Nothing -> error "no hostname for neil."
                    Just neil -> neil
        (_, files2, _) <- toDir $ dir t2
        channel <- connectTo hostname
        (flip evalStateT) oscarg $ do
            filesPrime2 <- mapKeysM nextShiftedPrime files2
        let ks2 = M.keys filesPrime2 
        g <- case seed config of
                Nothing -> error "no seed sent by Main."
                Just s -> return $ mkStdGen s
        let low = 2 ^ (pSize config) :: Integer
            high = 2 ^ (pSize config + 1) :: Integer
        hSetBuffering channel LineBuffering        
        let dowhile =
            (flip evalStateT) g $ do
                p <- makePrime low high 
            let pi2 = mkProduct p ks2
            hPutStrLn channel $ show pi2
            let neilData = hGetLine channel
            if neilData == ""
                then dowhile
                else 
                    
        dowhile
 
    Neil -> withSocketsDo $ do 
        args <- getArgs
        (config, t1, t2) <- parseArgs args
        socket <- listenOn port
        (channel, _, _) <- accept socket
        nielg <- getStdGen 
        (_, files1, _) <- toDir $ dir t1
        (flip evalStateT) nielg $ do
            filesPrime1 <- mapKeysM nextShiftedPrime files1
        let ks1 = M.keys filesPrime1
        g <- case seed config of
                Nothing -> error "no seed sent by Main."
                Just s -> return $ mkStdGen s
        let low = 2 ^ (pSize config) :: Integer
            high = 2 ^ (pSize config + 1) :: Integer
        hSetBuffering channel LineBuffering
        p <- makePrime low high
        
 
  
-- | This contains all the computations on Neil side for a round

roundN :: Integer -> Integer -> Integer -> Integer -> [Hash]
        -> (Integer, Integer, Maybe (Integer, [Hash]))
roundN oldD oldPs p pi2 ks1 =
    let newPs = p * oldPs
        pi1 = mkProduct p ks1
        d' = (pi1 * modularInv p pi2) `mod` p
        -- TODO: we hope that p is not a factor of modulo
        d = d' * modulo * modularInv p modulo + oldD * p * modularInv modulo p
        (a, b) = minFraction d newPs
        newHashes = detChanges a ks1
        okNew = (product newHashes `mod` newPs) == a
    in
    if okNew
        then (newPs, d, Just (b, newHashes))
        else (newPs, d, Nothing)

roundO :: Integer -> [Hash] -> Integer -> [Hash]
roundO b ks2 newPs =
    let deleteHashes = detChanges b ks2
    in deleteHashes
