module Processor where
import DataDefinitions
import PredicateParser 
import AxiomSchemes

data ValidateError = ValidateError Int String

validateProof :: LinedProof -> Either ValidateError Proof
validateProof = undefined

