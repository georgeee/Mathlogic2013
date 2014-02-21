module ErrorShowInstancesRussian where

import DataDefinitions

instance Show Warning where
    show (ReplacementWarning replacement target formula) = "терм " ++ (show replacement) ++ " не свободен для подстановки в формулу " ++ (show formula) ++ " вместо переменной " ++ (show target)
    show (InferenceRuleVarIsFreeWarning ruleId var formula) = "переменная " ++ (show var) ++ " входит свободно в формулу " ++ (show formula) ++ " в применении правила #" ++ (show ruleId)
    show (InferenceRuleAssumptionVarWarning ruleId var assumption) = "используется правило вывода #" ++ (show ruleId)
                                                                          ++ " с квантором по переменной " ++ (show var)
                                                                          ++ ", входящей свободно в допущение " ++ (show assumption)
    show (DSFormulaNotProvedError) = "Дедуктивное заключение не доказано"
instance Show Error where
    show (UndefinedError) = "Неизвестная ошибка"
    show (ParseError err) = "Ошибка парсинга " ++ err
    show (UndefinedValidateError ln) = "Неизвестная ошибка при валидации, строка" ++ (show $ ln)
    show (ValidateError ln ws) = "Вывод некорректен, начиная с формулы #" ++ (show $ ln + 1) ++ (printWarnings ws)
                                    where printWarnings [] = ""
                                          printWarnings (w:ws) = ": " ++ (show w)
    show (DSValidateError ws) = "Ошибка в заголовке дедукции" ++ (printWarnings ws)
                                    where printWarnings [] = ""
                                          printWarnings (w:ws) = ": " ++ (show w)
