module Main where
import Validator 
import DataDefinitions
import PredicateParser 
import FormulaReplace
import AxiomSchemes


readProofFromFile filename = do
    content <- readFile filename 
    return $ readProof content
    
readValidateProofFromFile filename = do
    _proof <- readProofFromFile filename
    return $ case _proof of Right proof -> validateProof proof

pe :: ValidateError -> String
pe (VError s) = s

rf = \s -> case readFormula s of Right f -> f
