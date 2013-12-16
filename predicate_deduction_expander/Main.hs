module Main where
import Processor
import DataDefinitions
import PredicateParser 


readProofFromFile filename = do
    content <- readFile filename 
    
