module ErrorShowInstancesDefault where

import DataDefinitions

instance Show Warning where
    show (ReplacementWarning replacement target formula) = "term " ++ (show replacement) ++ " isn't free to replace variable " ++ (show target) ++ " in formula " ++ (show formula)
    show (InferenceRuleVarIsFreeWarning ruleId var formula) = "variable " ++ (show var) ++ " is free in formula " ++ (show formula) ++ " in usage of rule #" ++ (show ruleId)
    show (InferenceRuleAssumptionVarWarning ruleId var assumption) = "usage of inference rule #" ++ (show ruleId)
                                                                          ++ " with quantor by variable " ++ (show var)
                                                                          ++ ", that is free inside assumption " ++ (show assumption)
    show (DSFormulaNotProvedError) = "Target formula wasn't proved"
instance Show Error where
    show (UndefinedError) = "Unknown error occured"
    show (ParseError err) = "Parsing error occured: " ++ err
    show (UndefinedValidateError ln) = "Unknown validate error occured on line " ++ (show $ ln)
    show (ValidateError ln ws) = "Validate error on line " ++ (show $ ln + 1) ++ ", warnings: " ++ (show ws)
    show (DSValidateError ws) = "Deduction statement validate error, warnings: " ++ (show ws)
