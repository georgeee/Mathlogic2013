module Main where
import Processor
import DataDefinitions
import PredicateParser 
import FormulaReplace
import AxiomSchemes


readProofFromFile filename = do
    content <- readFile filename 
    return $ readProof content
    


rf = \s -> case readFormula s of Right f -> f
