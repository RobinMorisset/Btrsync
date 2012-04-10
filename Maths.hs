{-# LANGUAGE ForeignFunctionInterface #-}
-- | Implements all the arithmetic part of the btrsync protocol
module Maths where

import Foreign
import Foreign.C.Types

import Data.Bits
import Math.Lattices.LLL

import Hashing (Hash(..))

-- returns i ^ j mod p
-- TODO: optimise further (#SPECIALIZE, and using divmod for example)
modExponent :: Integral i =>
    i -> i -> i -> i
modExponent i 0 p = 1
modExponent i j p | j `mod` 2 == 0 = 
    modExponent ((i * i) `mod` p) (j `div` 2) p
modExponent i j p | otherwise = 
    (i * i2) `mod` p
    where
        i2 = modExponent ((i * i) `mod` p) ((j - 1) `div` 2) p

-- Take the next prime for each h in hs, then
--  multiply them all together modulo p
mkProduct :: [Hash] -> Hash -> Hash
mkProduct hs p =
    let hs' = map (\ h -> nextPrime $ shiftL h 16) hs in
    foldl (\acc h -> (h * acc) `mod` p) 1 hs'
