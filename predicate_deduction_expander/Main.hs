module Main where
import Validator 
import DataDefinitions
import PredicateParser 
import FormulaReplace
import AxiomSchemes
import DeductionExpander
import "mtl" Control.Monad.State


readProofFromFile filename = do
    content <- readFile filename 
    return $ readProof content
    
readValidateProofFromFile filename = do
    _proof <- readProofFromFile filename
    return $ case _proof of Right proof -> validateProof proof
tryExpandDeductionFromFile filename = do
    _proof <- readValidateProofFromFile filename
    let (Right proof) = _proof in
        return $ tryExpandDeduction proof

pe :: ValidateError -> String
pe (VError s) = s

rf = \s -> case readFormula s of Right f -> f
