module Maths where

import Hashing (Hash(..))

foreign import "mpz_nextprime" nextPrime

-- Take the next prime for each h in hs, then
--  multiply them all together modulo p
mkProduct :: [Hash] -> Hash -> Hash
mkProduct hs p =
    undefined
