{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

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

nextShiftedPrime :: Integer -> Integer
nextShiftedPrime = nextPrime . (flip shiftL) 16

main :: IO ()
main = do
    args <- getArgs
    (config, t1, t2) <- parseArgs args
    g <- case seed config of
            Nothing -> getStdGen
            Just s -> return $ mkStdGen s
    let low = 2 ^ 1000 :: Hash
        high = 2 ^ 1001 :: Hash
        p = fst $ randomR (low, high) g
 
    -- Currently, we are only considering not-recursive dirs
    (_, files1, _) <- toDir (dir t1)
    let filesPrime1 = 
            M.mapKeys nextShiftedPrime files1
    let ks1 = M.keys filesPrime1
    let pi1 = mkProduct p ks1

    (_, files2, _) <- toDir (dir t2)
    let filesPrime2 = 
            M.mapKeys nextShiftedPrime files2
    let ks2 = M.keys filesPrime2 
    let pi2 = mkProduct p ks2
    
    putStr "p ="
    print p
    putStr "pi1 ="
    print pi1
    putStr "pi2 ="
    print pi2
                
    let d = (pi1 * modularInv p pi2) `mod` p
    let (a, b) = minFraction d p

    putStr "d ="
    print d
    putStr "a ="
    print a
    putStr "b ="
    print b

    let newHashes = detChanges a ks1
        deleteHashes = detChanges b ks2
        -- TODO: cleanup
        newFiles = map 
            (fromJust . ((flip M.lookup) filesPrime1)) 
            newHashes
        deleteFiles = map
            (fromJust . ((flip M.lookup) filesPrime2))
            deleteHashes
   
    putStr "NEW_HASHES ="
    print newHashes 
    print newFiles
    putStr "DELETE_HASHES ="
    print deleteHashes
    print deleteFiles

    exitSuccess
