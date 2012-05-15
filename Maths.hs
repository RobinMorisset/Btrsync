{-# LANGUAGE ForeignFunctionInterface #-}
-- | Implements all the arithmetic part of the btrsync protocol
module Maths where

import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.List
import Data.Array
import Data.Ratio
import Math.Lattices.LLL
import System.Random

import Hashing (Hash(..))

-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
  where 
    f k m
      | r == 1 = (k,m)
      | otherwise = f (k+1) q
        where (q,r) = quotRem m 2        
                      
-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Hash -> Hash -> Bool
millerRabinPrimality n a
  | a <= 1 || a >= n-1 = 
    error $ "millerRabinPrimality: a out of range (" 
    ++ show a ++ " for "++ show n ++ ")" 
  | n < 2 = False
  | even n = False
  | b0 == 1 || b0 == n' = True
  | otherwise = iter (tail b)
    where
      n' = n-1
      (k,m) = find2km n'
      b0 = powMod n a m
      b = take (fromIntegral k) $ iterate (squareMod n) b0
      iter [] = False
      iter (x:xs)
        | x == 1 = False
        | x == n' = True
        | otherwise = iter xs
                                    
-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
  where 
    f x n y
      | n == 1 = x `mul` y
      | r == 0 = f x2 q y
      | otherwise = f x2 q (x `mul` y)
        where
          (q,r) = quotRem n 2
          x2 = sq x
                       
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
 
-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

isPrime :: (RandomGen g, MonadState g m) => Hash -> m Bool
isPrime n =
  f n 10
  where
    f n k | k == 0 = return True
    f n k | otherwise = do
        g <- get
        let (a, g') = randomR (2, n-1) g
        put g'
        if millerRabinPrimality n a
            then f n (k - 1)
            else return False

nextPrime :: (RandomGen g, MonadState g m) => Hash -> m Hash
nextPrime n = liftM head $ filterM isPrime [n..]

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
mkProduct :: Hash -> [Hash] -> Hash
mkProduct p hs =
    foldl (\acc h -> (h * acc) `mod` p) 1 hs
    
modularInv :: Integral a => a -> a -> a
modularInv q 1 = 1
modularInv q p = (n * q + 1) `div` p
  where n = p - modularInv p (q `mod` p)

-- minimize a fraction modulo p
minFraction :: Hash -> Hash -> (Hash,Hash)
minFraction d p =
  let resultlll = lll [[1,0,p%1],[0,1,-p*d%1],[0,0,p*p%1]]
      result = resultlll!0
      a = head result
      b = head (tail result) in
  (numerator (abs a),numerator (abs b))

pgcd :: Hash -> Hash -> Hash
pgcd 0 k = k
pgcd k 0 = k
pgcd a b = pgcd c (d `mod` c)
           where d = max a b
                 c = min a b

detChanges :: Hash -> [Hash] -> [Hash]
detChanges x l = filter (\ y -> pgcd x y /= 1) l

