module DataDefinitions where

data Var = Var String
    deriving (Eq, Ord)
data Term = VarTerm Var | FunctionalTerm String [Term]
    deriving (Eq, Ord)
data Formula = Predicate String [Term]
    |Not Formula
    |And Formula Formula
    |Or Formula Formula
    |Impl Formula Formula
    |Exists Var Formula
    |ForAll Var Formula
    deriving (Eq, Ord)

data DeductionStatement = DeductionStatement {
        dsConditions :: [Formula],
        dsFormula    :: Formula
    }
    deriving Eq
data Proof = Proof { ds :: DeductionStatement, fList :: [Formula] }
    deriving Eq
data LinedProof = LinedProof DeductionStatement [(Int,Formula)]
    deriving Eq

(-|-) f1 f2 = Or f1 f2
infixl 2 -|-

(->-) f1 f2 = Impl f1 f2
infixr 1 ->-

(-&-) f1 f2 = And f1 f2
infixl 3 -&- 

instance Show Var where
    show (Var s) = s

print_subterms :: String -> [Term] -> String
print_subterms name [] = name
print_subterms name subterms = name ++ "(" ++ (psubs subterms) ++ ")"
        where psubs (subterm:[]) = show subterm
              psubs (subterm: subterms) = (show subterm) ++ ", " ++ (psubs subterms)
 

instance Show Term where
    show (VarTerm var) = show var
    show (FunctionalTerm name subterms) = print_subterms name subterms
instance Show Formula where
    show (Predicate name subterms) = print_subterms name subterms
    show (Not unary) = "(!" ++ (show unary) ++ ")"
    show (ForAll var unary) = "(@"++(show var)++" "++(show unary) ++ ")"
    show (Exists var unary) = "(?"++(show var)++" "++(show unary) ++ ")"
    show (Impl a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
    show (And a b) = "(" ++ (show a) ++ " & " ++ (show b) ++ ")"
    show (Or a b) = "(" ++ (show a) ++ " | " ++ (show b) ++ ")"
instance Show DeductionStatement where
    show ds = (print_conds $ dsConditions ds) ++ " |- " ++ (show $ dsFormula ds)
        where
            print_conds (a:[]) = show a
            print_conds (a:as) = (show a) ++ ", " ++ (print_conds as)
            print_conds [] = ""
instance Show Proof where
    show p = foldr1 (\a -> \b -> a ++ "\n" ++ b) $ (show $ ds p) : (map show $ fList p)
instance Show LinedProof where
    show (LinedProof ds fs) = foldr1 (\a -> \b -> a ++ "\n" ++ b) $ (show $ ds) : (map (show.snd) $ fs)

unLineProof :: LinedProof -> Proof
unLineProof (LinedProof ds fs) = Proof ds $ map snd fs

data Warning = ReplacementWarning {replacement :: Term, target :: Var, formula :: Formula}
             | DeductionAssumptionWarning {var :: Var, assumption :: Formula}
             | AxiomSchemeUseWarning {axiomSchemeId :: Int, var :: Var, formula :: Formula}
             | InferenceRuleUseWarning {ruleId :: Int, var :: Var, formula :: Formula}
             | DSFormulaNotProvedError
instance Show Warning where
    show (ReplacementWarning replacement target formula) = "Variable " ++ (show target) ++ " isn't free for replacement by term " ++ (show replacement) ++ " in formula " ++ (show formula)
    show (DeductionAssumptionWarning var assumption) = "Variable " ++ (show var) ++ " is free in formula " ++ (show assumption)
    show (AxiomSchemeUseWarning axiomSchemeId var formula) = "Use of axiom scheme #" ++ (show axiomSchemeId) ++ " with quantor by var " ++ (show var) ++ ", that is free inside " ++ (show formula)
    show (InferenceRuleUseWarning ruleId var formula) = "Use of inference rule #" ++ (show ruleId) ++ " with quantor by var " ++ (show var) ++ ", that is free inside " ++ (show formula)
    show (DSFormulaNotProvedError) = "Target formula wasn't proved"
data Error = UndefinedError | ParseError String
             | UndefinedValidateError Int
             | ValidateError Int [Warning]
             | DSValidateError [Warning]
instance Show Error where
    show (UndefinedError) = "Unknown error occured"
    show (ParseError err) = "Parsing error occured: " ++ err
    show (UndefinedValidateError ln) = "Unknown validate error occured on line " ++ (show $ ln)
    show (ValidateError ln ws) = "Validate error on line " ++ (show $ ln + 1) ++ ", warnings: " ++ (show ws)
    show (DSValidateError ws) = "Deduction statement validate error, warnings: " ++ (show ws)
