module DataDefinitions where

data Var = Var String
data Term = VarTerm Var | FunctionalTerm String [Term]
data Formula = Predicate String [Term]
    |Not Formula
    |And Formula Formula
    |Or Formula Formula
    |Impl Formula Formula
    |Exists Var Formula
    |ForAll Var Formula
type ErrorMsg = String
data DeductionStatement = DeductionStatement {
    dsConditions :: [Formula],
    dsFormula    :: Formula
}
data Proof = Proof { ds :: DeductionStatement, fList :: [Formula] }

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
    show p = foldr (\a -> \b -> a ++ "\n" ++ b) (show $ ds p) $ map show $ fList p

