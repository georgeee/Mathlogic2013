module ArithmeticAxioms where
import DataDefinitions
import "mtl" Control.Monad.Writer
import qualified Data.Map as M
import FormulaReplace
import Data.Maybe
import Parser

a = VarTerm $ Var "a"
b = VarTerm $ Var "b"
c = VarTerm $ Var "c"
c0 = FunctionalTerm "0" []
n t = FunctionalTerm "'" [t]


testArithmeticAxiom1 = (==) $ a -=- b ->- (n a) -=- (n b)
testArithmeticAxiom2 = (==) $ a -=- b ->- a -=- c ->- b -=- c
testArithmeticAxiom3 = (==) $ (n a) -=- (n b) ->- a -=- b
testArithmeticAxiom4 = (==) $ Not ((n a) -=- c0)
testArithmeticAxiom5 = (==) $ a -+- (n b) -=- (n $ a -+- b)
testArithmeticAxiom6 = (==) $ a -+- c0 -=- a
testArithmeticAxiom7 = (==) $ a -*- c0 -=- c0
testArithmeticAxiom8 = (==) $ a -*- (n b) -=- a -*- b -+- a

simpleArithmeticAxiomList = [testArithmeticAxiom1, testArithmeticAxiom2, testArithmeticAxiom3, testArithmeticAxiom4, testArithmeticAxiom5, testArithmeticAxiom6, testArithmeticAxiom7, testArithmeticAxiom8]

testArithmeticAxiom9 :: DSFreeVarsMap -> Formula -> Writer [Warning] Bool
testArithmeticAxiom9 vars (Impl a@(And _ (ForAll var _)) f)
      = do _f0 <- replace' f var c0
           _f' <- replace' f var (n $ VarTerm var)
           notFree <- checkNotInVars var vars "A9"
           return $ (not notFree) && (isJust $
                             do f0 <- _f0
                                f' <- _f'
                                return $ if a == (f0 -&- ForAll var (f ->- f'))
                                         then Just () else Nothing)
testArithmeticAxiom9 _ _ = return False

arithmeticAxiomList = (map (\func _ f -> return $ func f) simpleArithmeticAxiomList) ++ [testArithmeticAxiom9]

