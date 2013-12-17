module AxiomSchemes where
import DataDefinitions
import FormulaReplace

testAxiomScheme1 :: Formula -> Bool
testAxiomScheme1 (Impl a (Impl b c)) = a==c
testAxiomScheme1 _ = False  

testAxiomScheme2 :: Formula -> Bool
testAxiomScheme2 (Impl (Impl a1 b1) (Impl (Impl a2 (Impl b2 c2)) (Impl a3 c3)))
                            = a1 == a2 && b1 == b2 && a1 == a3 && c2 == c3
testAxiomScheme2 _ = False

testAxiomScheme3 :: Formula -> Bool
testAxiomScheme3 (Impl a (Impl b (And _a _b))) = a == _a && b == _b
testAxiomScheme3 _ = False

testAxiomScheme4 :: Formula -> Bool
testAxiomScheme4 (Impl (And a b) _a) = a == _a
testAxiomScheme4 _ = False

testAxiomScheme5 :: Formula -> Bool
testAxiomScheme5 (Impl (And a b) _b) = b == _b
testAxiomScheme5 _ = False

testAxiomScheme6 :: Formula -> Bool
testAxiomScheme6 (Impl _a (Or a b)) = _a == a
testAxiomScheme6 _ = False

testAxiomScheme7 :: Formula -> Bool
testAxiomScheme7 (Impl _b (Or a b)) = _b == b
testAxiomScheme7 _ = False

testAxiomScheme8 :: Formula -> Bool
testAxiomScheme8 (Impl (Impl a1 b1) (Impl (Impl c2 b2) (Impl (Or a3 c3) b3)))
                            = a1 == a3 && c2 == c3 && b1 == b2 && b1 == b3
testAxiomScheme8 _ = False

testAxiomScheme9 :: Formula -> Bool
testAxiomScheme9 (Impl (Impl a1 b1) (Impl (Impl a2 (Not b2)) (Not a3)))
                            = a1 == a2 && a1 == a3 && b1 == b2
testAxiomScheme9 _ = False

testAxiomScheme10 :: Formula -> Bool
testAxiomScheme10 (Impl (Not (Not a)) _a) = a == _a
testAxiomScheme10 _ = False

testAxiomScheme11 :: Formula -> Bool
testAxiomScheme11 (Impl (ForAll x f1) f2) = checkReplEq f1 f2 x
testAxiomScheme11 _ = False

testAxiomScheme12 :: Formula -> Bool
testAxiomScheme12 (Impl f2 (Exists x f1)) = checkReplEq f1 f2 x
testAxiomScheme12 _ = False
