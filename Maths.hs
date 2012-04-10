module Maths where

import Math.Lattices.LLL



import Hashing (Hash(..))

-- Take the next prime for each h in hs, then
--  multiply them all together modulo p
mkProduct :: [Hash] -> Hash -> Hash
mkProduct hs p =
    undefined
