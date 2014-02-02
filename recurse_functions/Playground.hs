{-# LANGUAGE TemplateHaskell #-}
module Playground where

import Primitives
import Basics
import Functions
import DataTypes
import Language.Haskell.TH

testPrimes n = putStr $ tp n
    where tp n = (if n == 0 then "" else tp $ n-1) 
               ++ (show n) ++ ": "
               ++ (show $ isPrime $ fromInteger n)
               ++ "\n"
a1 = 1 @+ 4
a2 = 2 @- 9
a3 = 9 @- 2
a4 = 5 @* 3
a5 = 2 @^ 5
a6 = 17 `nDiv` 4
a7 = 17 `nMod` 4
a8 = 2 `nDiff` 5
a9 = plog 2 48

n5thPrime = nthPrime 5
