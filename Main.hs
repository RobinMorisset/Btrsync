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

    (flip evalStateT) g $ do
        let low = 2 ^ 300 :: Hash
            high = 2 ^ 301 :: Hash
        
        filesPrime1 <- mapKeysM nextShiftedPrime files1
        filesPrime2 <- mapKeysM nextShiftedPrime files2
        let ks1 = M.keys filesPrime1
        let ks2 = M.keys filesPrime2 

        lift $ print "DEBUG_BEFORE_WHILENOT"       
 
        let whilenot :: Integer -> Integer -> StateT StdGen IO ([File], [File])
            whilenot oldD modulo = do
            g <- get
            p <- nextPrime . fst $ randomR (low, high) g
            let newP = p * modulo 

            -- Currently, we are only considering not-recursive dirs
            let pi1 = mkProduct p ks1
            let pi2 = mkProduct p ks2
        
            lift $ do 
                putStr "p ="
                print p
                putStr "newP="
                print newP
                putStr "pi1 ="
                print pi1
                putStr "pi2 ="
                print pi2
            
            let d' = (pi1 * modularInv p pi2) `mod` p 
            let d = crt [(d',p), (oldD, modulo)]
            let (a, b) = minFraction d newP
            
            lift $ do
                putStr "d ="
                print d
                putStr "a ="
                print a
                putStr "b ="
                print b

            let newHashes = detChanges a ks1
                deleteHashes = detChanges b ks2
                pNewHashes = product newHashes `mod` newP
                okNew = pNewHashes == a
                pDeleteHashes = product deleteHashes `mod` newP
                okDelete = pDeleteHashes == b
                -- TODO: cleanup
                foldaux :: (M.Map Hash File) -> [File] -> Hash -> [File]
                foldaux filesPrime files h = 
                    let maybeFile = M.lookup h filesPrime in
                    case maybeFile of
                        Nothing -> files
                        Just file -> file : files
                newFiles = 
                    foldl (foldaux filesPrime1) [] newHashes
                deleteFiles = 
                    foldl (foldaux filesPrime2) [] deleteHashes

            lift $ do
                putStr "NEW_HASHES ="
                print newHashes 
                print newFiles
                putStr "DELETE_HASHES ="
                print deleteHashes
                print deleteFiles           
 
            if (okNew && okDelete) 
                then return (newFiles, deleteFiles)
                else whilenot d newP
        
        _ <- whilenot 0 1
        return ()
    exitSuccess
