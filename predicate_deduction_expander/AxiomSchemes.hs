module AxiomSchemes(isAxiom, getAxiomId, getAxiomId') where
import DataDefinitions
import "mtl" Control.Monad.Writer
import qualified Data.Map as M
import FormulaReplace
import Data.Maybe

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

testWarningAxiomScheme f1 f2 x vars id = do res <- checkEqualAfterReplacement f1 f2 x
                                            res2 <- checkNotInVars x vars
                                            return $ res && res2
                                        where checkNotInVars x vars = if M.member x vars
                                                                      then do tell [AxiomSchemeAssumptionVarWarning id x $ vars M.! x]
                                                                              return False
                                                                      else return True

testAxiomScheme11 :: DSFreeVarsMap -> Formula -> Writer [Warning] Bool
testAxiomScheme11 vars (Impl (ForAll x f1) f2) = testWarningAxiomScheme f1 f2 x vars 11
testAxiomScheme11 _ _ = return False

testAxiomScheme12 :: DSFreeVarsMap -> Formula -> Writer [Warning] Bool
testAxiomScheme12 vars (Impl f2 (Exists x f1)) = testWarningAxiomScheme f1 f2 x vars 12
testAxiomScheme12 _ _ = return False

simpleAxiomSchemeList = [testAxiomScheme1, testAxiomScheme2, testAxiomScheme3,
                         testAxiomScheme4, testAxiomScheme5, testAxiomScheme6,
                         testAxiomScheme7, testAxiomScheme8, testAxiomScheme9,
                         testAxiomScheme10]
warningAxiomSchemeList = [testAxiomScheme11, testAxiomScheme12]

axiomSchemeList = (map transformer simpleAxiomSchemeList) ++ warningAxiomSchemeList
    where transformer :: (Formula -> Bool) -> (DSFreeVarsMap -> Formula -> Writer [Warning] Bool)
          transformer func = \_ -> \f -> return $ func f

{-testAxiomScheme :: Int -> DSFreeVarsMap -> Formula -> Writer [Warning] Bool-}
{-testAxiomScheme = (axiomSchemeList !!) . ((-) 1)-}

getAxiomId :: DSFreeVarsMap -> Formula -> Maybe Int
getAxiomId vars f = let (result, ws) = runWriter $ getAxiomId' vars f
               in result

getAxiomId' :: DSFreeVarsMap -> Formula -> Writer [Warning] (Maybe Int)
getAxiomId' = impl 1 axiomSchemeList
    where impl :: Int -> [DSFreeVarsMap -> Formula -> Writer [Warning] Bool] -> DSFreeVarsMap -> Formula -> Writer [Warning] (Maybe Int)
          impl _ [] _ _ = return Nothing
          impl n (test:rest) vars f = do
                isOK <- test vars f
                if isOK
                    then return $ Just n
                    else impl (n+1) rest vars f

isAxiom :: DSFreeVarsMap -> Formula -> Bool
isAxiom vars f = isJust $ getAxiomId vars f
