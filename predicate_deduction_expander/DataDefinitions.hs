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

data Error = NumberedVError Int String | NullError | VError String | ParseError String
instance Show Error where
    show (NumberedVError n msg) = "Validate error on line #" ++ (show n) ++ ": " ++ msg
    show (VError msg) = "Validate error: " ++ msg
    show (ParseError msg) = "Parse error: " ++ msg
    show (NullError) = "Unknown error occured"
