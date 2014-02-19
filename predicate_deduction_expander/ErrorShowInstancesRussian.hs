module ErrorShowInstancesRussian where

import DataDefinitions

instance Show Warning where
    show (ReplacementWarning replacement target formula) = "терм " ++ (show replacement) ++ " не свободен для подстановки в формулу " ++ (show formula) ++ " вместо переменной " ++ (show target)
    show (InferenceRuleVarIsFreeWarning ruleId var formula) = "переменная " ++ (show var) ++ " входит свободно в формулу " ++ (show formula) ++ " в применении правила " ++ (show ruleId)
    show (AxiomSchemeAssumptionVarWarning axiomSchemeId var assumption) = "используется схема аксиом #" ++ (show axiomSchemeId)
                                                                          ++ " с квантором по переменной " ++ (show var)
                                                                          ++ ", входящей свободно в допущение " ++ (show assumption)
    show (InferenceRuleAssumptionVarWarning ruleId var assumption) = "используется правило вывода #" ++ (show ruleId)
                                                                          ++ " с квантором по переменной " ++ (show var)
                                                                          ++ ", входящей свободно в допущение " ++ (show assumption)
    show (DSFormulaNotProvedError) = "Target formula wasn't proved"
instance Show Error where
    show (UndefinedError) = "Unknown error occured"
    show (ParseError err) = "Parsing error occured: " ++ err
    show (UndefinedValidateError ln) = "Unknown validate error occured on line " ++ (show $ ln)
    show (ValidateError ln ws) = "Validate error on line " ++ (show $ ln + 1) ++ ", warnings: " ++ (show ws)
    show (DSValidateError ws) = "Deduction statement validate error, warnings: " ++ (show ws)
