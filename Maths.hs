module Maths where

import Math.Lattices.LLL
import Data.List
import Data.Array
import Data.Ratio

import Hashing (Hash(..))

-- Take the next prime for each h in hs, then
--  multiply them all together modulo p
mkProduct :: [Hash] -> Hash -> Hash
mkProduct hs p =
    undefined

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
