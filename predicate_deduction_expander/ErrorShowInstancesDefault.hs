module ErrorShowInstancesDefault where

import DataDefinitions

instance Show Warning where
    show (ReplacementWarning replacement target formula) = "Variable " ++ (show target) ++ " isn't free for replacement by term " ++ (show replacement) ++ " in formula " ++ (show formula)
    show (DeductionAssumptionWarning var assumption) = "Variable " ++ (show var) ++ " is free in formula " ++ (show assumption)
    show (AxiomSchemeUseWarning axiomSchemeId var formula) = "Use of axiom scheme #" ++ (show axiomSchemeId) ++ " with quantor by var " ++ (show var) ++ ", that is free inside " ++ (show formula)
    show (InferenceRuleUseWarning ruleId var formula) = "Use of inference rule #" ++ (show ruleId) ++ " with quantor by var " ++ (show var) ++ ", that is free inside " ++ (show formula)
    show (DSFormulaNotProvedError) = "Target formula wasn't proved"
instance Show Error where
    show (UndefinedError) = "Unknown error occured"
    show (ParseError err) = "Parsing error occured: " ++ err
    show (UndefinedValidateError ln) = "Unknown validate error occured on line " ++ (show $ ln)
    show (ValidateError ln ws) = "Validate error on line " ++ (show $ ln + 1) ++ ", warnings: " ++ (show ws)
    show (DSValidateError ws) = "Deduction statement validate error, warnings: " ++ (show ws)
