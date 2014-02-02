{-# LANGUAGE TemplateHaskell #-}
module Functions where

import Primitives
import Basics
import DataTypes
import Language.Haskell.TH

-- + = R<U_1^1, S<N, U_3^3>>
(@+) :: Nat -> Nat -> Nat
(@+) = $(r 1) $(u 1 1) ($(s 1 3) n $(u 3 3))
infixl 6 @+

-- - = R<U_1^1, S<P, U_3^3>>
(@-) :: Nat -> Nat -> Nat
(@-) = $(r 1) $(u 1 1) ($(s 1 3) p $(u 3 3))
infixl 6 @-

-- * = R<U_1^1, S<+, U_1^3, U_3^3>>
(@*) :: Nat -> Nat -> Nat
(@*) = $(r 1) z ($(s 2 3) (@+) $(u 3 1) $(u 3 3))
infixl 7 @*

-- '=' = Not<S<ifFalse<->, U_2^2, U_1^2, S<-, U_1^2, U_2^2>>>
(@=) :: Nat -> Nat -> Nat
(@=) = $(rfNot 2) ($(s 3 2) ($(ifFalse 2) (@-)) $(u 2 2) $(u 2 1) (@-))
infixl 1 @=

-- div(x,y) = S<isP<S<Z, U_1^2>,g>, U_1^2, U_2^2, S<P, S<-, U_2^2, U_1^2>>>
-- g = S<R<S<Z, U_1^2>, g'>, U_1^2, U_2^2, S<N, U_1^2>>
-- //g works fine, we put it in bigger construction for a bit of performance in
-- // some cases
-- //iterate a in [0..x]
-- g'(x,y,a,prev) = ifFalse<S<g'', U_1^3, U_2^3, U_3^3,U_2^3>>
-- //prev - if we succeed with one of previous a then a, 0 otherwise
-- //if prev >0 then prev else g''(x,y,a,y)
-- g'' = R<S<Z, U_1^3>, ifFalse<g'''>>
-- //iterate b in [0..y-1]. if one y succeed, it's beeing returned, otherwise
-- //returning g'''(x,y,a,b)
-- g''' = S<isP<U_1^1, Z>, U_3^4, S<=, U_1^4, S<+, S<*, U_2^4, U_3^4>, U_4^4>>>
-- //if (x=a*y+b), b<y then return a else return 0
nDiv :: Nat -> Nat -> Nat
nDiv = $(s 3 2) ($(isP 2) ($(s 1 2) z $(u 2 1)) g) $(u 2 1) $(u 2 2) ($(s 1 2) p ($(s 2 2) (@-) $(u 2 2) $(u 2 1)))
    where g  = $(s 3 2) ($(r 2) ($(s 1 2) z $(u 2 1)) g') $(u 2 1) $(u 2 2) ($(s 1 2) n $(u 2 1)) 
          g' = $(ifFalse 3) ($(s 4 3) g'' $(u 3 1) $(u 3 2) $(u 3 3) $(u 3 2))
          g'' = $(r 3) ($(s 1 3) z $(u 3 1)) ($(ifFalse 4) g''')
          g''' = $(s 2 4) ($(isP 1) $(u 1 1) z) $(u 4 3) g4
          g4 = $(s 2 4) (@=) $(u 4 1) g5
          g5 = $(s 2 4) (@+) $(u 4 4) g6
          g6 = $(s 2 4) (@*) $(u 4 2) $(u 4 3)
infixl 7 `nDiv`

-- mod = S<-, U_1^2, S<*, U_2^2, S<div, U_1^2, U_2^2>>>
nMod :: Nat -> Nat -> Nat
nMod = $(s 2 2) (@-) $(u 2 1) ($(s 2 2) (@*) $(u 2 2) ($(s 2 2) nDiv $(u 2 1) $(u 2 2)))
infixl 7 `nMod`

-- diff = S<ifFalse<->, U_2^2, U_1^2, S<-, U_1^2, U_2^2>>
nDiff = $(s 3 2) ($(ifFalse 2) (@-)) $(u 2 2) $(u 2 1) ($(s 2 2) (@-) $(u 2 1) $(u 2 2))
infixl 6 `nDiff`

-- isPrime = Not<isNotPrime>
isPrime = $(rfNot 1) isNotPrime

-- isNotPrime' = S<R<Z, ifFalse<S<mod,U_1^2, S<S<N,N>, U_2^2>>>>, U_1^1, S<P, P>>
-- //correct test for x>=2
-- isNotPrime = S<isP<isNotPrime', S<N, Z>>, U_1^1, P>
-- //correct for any x
isNotPrime = test
    where test' = $(s 2 1) ($(r 1) z ($(ifFalse 2) $ $(s 2 2) ($(rfNot 2) nMod) $(u 2 1) ($(s 1 2) ($(s 1 1) n n) $(u 2 2)))) $(u 1 1) ($(s 1 1) p p)
          test  = $(s 2 1) ($(isP 1) test' ($(s 1 1) n z)) $(u 1 1) p

-- nthPrime = M<g>
-- g = S<diff, U_1^2, S<S<R<Z,g''>,U_1^1,U_1^1>,U_2^2>
-- g'' = S<isP<N, U_1^1>, U_3^3, S<isPrime, S<N, U_2^3>>>
nthPrime = $(m 1) g
    where g = $(s 2 2) nDiff $(u 2 1) ($(s 2 2) ($(r 1) z g'') $(u 2 2) $(u 2 2))
          g'' = $(s 2 3) ($(isP 1) n $(u 1 1)) $(u 3 3) ($(s 1 3) isPrime ($(s 1 3) n $(u 3 2)))


-- @^ = R<U_1^2, S<*, U_1^4, U_4^4>>
(@^) :: Nat -> Nat -> Nat
(@^) = $(r 1) ($(s 1 1) n z) ($(s 2 3) (@*) $(u 3 1) $(u 3 3))
infixr 8 @^

-- plog = M<g'>
-- g'(p, x, r) = S<Not<mod>, U_2^3, S<^, U_1^3, S<N, U_3^3>>>
-- r - max log, when x mod p^r = 0
plog = $(m 2) $ $(s 2 3) ($(rfNot 2) nMod) $(u 3 2) $ $(s 2 3) (@^) $(u 3 1) $ $(s 1 3) n $(u 3 3)


-- (plog 2 192) works really long
-- so that I've tried to measure, how will it be using native integer mod and pow implementations
-- so that I've created plog'
plog' = $(m 2) $ $(s 2 3) ($(rfNot 2) $ evalInteger mod) $(u 3 2) $ $(s 2 3) (evalInteger (^)) $(u 3 1) $ $(s 1 3) n $(u 3 3)
evalInteger :: (Integer -> Integer -> Integer) -> Nat -> Nat -> Nat
evalInteger f x y = fromInteger $ f (toInteger x) (toInteger y)



