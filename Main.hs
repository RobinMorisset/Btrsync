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
    g <- case seed config of
            Nothing -> getStdGen
            Just s -> return $ mkStdGen s
    (_, files1, _) <- toDir $ dir t1
    (_, files2, _) <- toDir $ dir t2

    (newFiles, deleteFiles) <- (flip evalStateT) g $ do
        let low = 2 ^ 300 :: Hash
            high = 2 ^ 301 :: Hash
        
        filesPrime1 <- mapKeysM nextShiftedPrime files1
        filesPrime2 <- mapKeysM nextShiftedPrime files2
        let ks1 = M.keys filesPrime1
        let ks2 = M.keys filesPrime2 

        let whilenot :: Integer -> Integer -> StateT StdGen IO ([Hash], [Hash])
            whilenot oldD modulo = do
            g <- get
            let (preP, g') = randomR (low, high) g
            put g'
            p <- nextPrime preP
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
