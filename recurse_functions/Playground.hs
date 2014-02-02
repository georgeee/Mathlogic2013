{-# LANGUAGE TemplateHaskell #-}
module Playground where

import Primitives
import Basics
import Functions
import DataTypes
import Language.Haskell.TH

u_3_2 = $(u 3 2)
u_4_2 = $(u 4 2)

zero = Zero
one = Succ zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four


s_1_2 = $(s 1 2)

testPrimes n = putStr $ tp n
    where tp n = (if n == 0 then "" else tp $ n-1) 
               ++ (show n) ++ ": "
               ++ (show $ isPrime $ fromInteger n)
               ++ "\n"

aa = $(m 1) nDiff
b1 = $(s 1 3) n $(u 3 3)
b2 = $(s 2 3) (@^) $(u 3 1) b1
bb = $(s 2 3) (nMod) $(u 3 2) b2
