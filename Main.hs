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

port :: PortID
port = undefined

main :: IO ()
main = do 
    args <- getArgs
    (config, t1, t2) <- parseArgs args        
    g <- case seed config of
        Nothing -> error "no seed sent by Main."
        Just s -> return $ mkStdGen s
    let low = 2 ^ (pSize config) :: Integer
        high = 2 ^ (pSize config + 1) :: Integer
    case role config of
	Main -> undefined 

        Oscar -> withSocketsDo $ do
            oscarg <- getStdGen
            let maybeneil = host t1
                hostname = case maybeneil of
                        Nothing -> error "no hostname for neil."
                        Just neil -> neil
            (_, files2, _) <- toDir (dir t2) ""
            filesPrime2 <- (flip evalStateT) oscarg $ 
                mapKeysM nextShiftedPrime files2
            channel <- connectTo hostname port
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
                        undefined 
            dowhile g
    
        Neil -> withSocketsDo $ do 
            socket <- listenOn port
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
