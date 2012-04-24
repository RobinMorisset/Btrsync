{-# LANGUAGE ForeignFunctionInterface #-}
-- | Implements all the arithmetic part of the btrsync protocol
module Maths where

import Foreign
import Foreign.C.Types

import Math.Lattices.LLL
import Data.Bits
import Data.List
import Data.Array
import Data.Ratio

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

modularInv :: Integral a => a -> a -> a
modularInv q 1 = 1
modularInv q p = (n * q + 1) `div` p
  where n = p - modularInv p (q `mod` p)

minFraction :: Hash -> Hash -> (Hash,Hash)
minFraction d p = 
  let resultlll = lll [[1,0,0,-1],[0,1,0,d%1],[0,0,1,-p%1]] in
  let result = resultlll!1 in
  let a = head result + last result in
  let b = head (tail result) in
  if (a * b > 0) then
    (numerator (abs a), numerator (abs b))
  else
    let result = resultlll!2 in  
    let a = head result + last result in
    let b = head (tail result) in
    (numerator (abs a), numerator (abs b))

pgcd :: Hash -> Hash -> Hash
pgcd 0 k = k
pgcd k 0 = k
pgcd a b = pgcd c (d `mod` c)
           where d = max a b
                 c = min a b

detChanges :: Hash -> [Hash] -> [Hash]
detChanges a [] = []
detChanges a list =
  let b = head list in
  let tl = tail list in
  if (pgcd a b == 1) 
  then b : (detChanges a tl)
  else (detChanges a tl)