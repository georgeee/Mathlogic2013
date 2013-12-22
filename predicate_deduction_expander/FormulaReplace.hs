module FormulaReplace(findFree, isFree, isFreeInTerm, vars, varSet, replace, replace', replaceInTerm, checkReplEq,findFirstStructureMatching) where
import DataDefinitions
import "mtl" Control.Monad.Writer
import qualified Data.Set as Set

isFree :: Formula -> Var -> Bool
isFree (Predicate name terms) = \x -> all (isFreeInTerm x) terms
isFree (Not f)      = isFree f
isFree (And a b)    = \x -> (isFree a x) && (isFree b x)
isFree (Or a b)     = \x -> (isFree a x) && (isFree b x)
isFree (Impl a b)   = \x -> (isFree a x) && (isFree b x)
isFree (Exists v f) = \x -> if v==x then False else isFree f x 
isFree (ForAll v f) = \x -> if v==x then False else isFree f x 

findFree :: Formula -> Maybe Var
findFree f = findFreeImpl f Set.empty
    where findFreeImpl (Predicate _ terms) = \set -> let intersection = (foldr1 Set.union $ map varSet terms) Set.\\ set
                                                 in if Set.null intersection then Nothing else Just $ Set.findMin intersection
          findFreeImpl (Not f)      = findFreeImpl f
          findFreeImpl (And a b)    = a <|> b
          findFreeImpl (Or a b)     = a <|> b
          findFreeImpl (Impl a b)   = a <|> b
          findFreeImpl (Exists v f) = \set -> findFreeImpl f (Set.insert v set)
          findFreeImpl (ForAll v f) = \set -> findFreeImpl f (Set.insert v set)
          (<|>) a b set = case findFreeImpl a set of
                              Nothing -> findFreeImpl b set
                              m -> m
          

isFreeInTerm x (VarTerm v) = v /= x
isFreeInTerm x (FunctionalTerm _ terms) = all (isFreeInTerm x) terms

varSet :: Term -> Set.Set Var
varSet term = varsImpl term Set.empty
    where varsImpl (VarTerm var) = Set.insert var
          varsImpl (FunctionalTerm _ terms) = varsImplTs terms
          varsImplTs [] = id
          varsImplTs (t:ts) = \set -> varsImplTs ts $ varsImpl t set
vars = Set.toList . varSet

replace :: Formula -> Var -> Term -> Maybe Formula
replace f v t = let (result, ws) = runWriter $ replace' f v t
                in result

replace' :: Formula -> Var -> Term -> Writer [Warning] (Maybe Formula)
replace' (Predicate name terms) = \x -> \y -> return $ Just $ Predicate name $ map (replaceInTerm x y) terms
replace' (Not f)      = replaceUnaryImpl   Not    f
replace' (And a b)    = replaceBinaryImpl  And    a b
replace' (Or a b)     = replaceBinaryImpl  Or     a b
replace' (Impl a b)   = replaceBinaryImpl  Impl   a b
replace' (Exists v f) = replaceQuantorImpl Exists v f
replace' (ForAll v f) = replaceQuantorImpl ForAll v f 

replaceBinaryImpl constructor f1 f2 x y
        = do subRes1 <- replace' f1 x y
             subRes2 <- replace' f2 x y
             return $ do
                        s1 <- subRes1
                        s2 <- subRes2
                        return $ constructor s1 s2
replaceUnaryImpl constructor f x y
        = do subRes <- replace' f x y
             return $ do s <- subRes
                         return $ constructor s
replaceQuantorImpl :: (Var -> Formula -> Formula) -> Var -> Formula -> Var -> Term -> Writer [Warning] (Maybe Formula)
replaceQuantorImpl constructor v f x y
        = if (v == x) then return $ Just $ constructor v f
                      else if (Set.member v (varSet y))
                           then do
                                 tell [ReplacementWarning y x (constructor v f)]
                                 return Nothing
                           else do
                                 r <- replace' f x y
                                 return $ do r' <- r
                                             return $ constructor v r'

replaceInTerm :: Var -> Term -> Term -> Term
replaceInTerm x y term@(VarTerm var) = if (var == x) then y else term
replaceInTerm x y (FunctionalTerm name terms)
                = FunctionalTerm name $ map (replaceInTerm x y) terms


findFirstStructureMatching :: Formula -> Formula -> Var -> Maybe Term
findFirstStructureMatching f1 f2 var = case (ffsmImpl f1 f2 var) of
      Nothing -> Nothing
      Just Nothing -> Nothing
      Just res -> res
    where ffsmImpl :: Formula -> Formula -> Var -> Maybe (Maybe Term)
          ffsmImpl (Predicate name1 terms1) (Predicate name2 terms2)
                    = if(name1 /= name2) then \_ -> Just Nothing else checkTermLists terms1 terms2
          ffsmImpl (Not xF) (Not yF) = ffsmImpl xF yF
          ffsmImpl (And a1 b1) (And a2 b2) = binaryImpl a2 b1 a2 b2
          ffsmImpl (Or a1 b1) (Or a2 b2) = binaryImpl a2 b1 a2 b2
          ffsmImpl (Impl a1 b1) (Impl a2 b2) = binaryImpl a2 b1 a2 b2
          ffsmImpl (ForAll x xF) (ForAll y yF) = quantorImpl x y xF yF
          ffsmImpl (Exists x xF) (Exists y yF) = quantorImpl x y xF yF
          ffsmImpl _ _ = \_ -> Just Nothing

          checkVars x y var = if (var == x) then Just $ Just y else Nothing

          quantorImpl x y xF yF var = case (checkVars x (VarTerm y) var) of
                    Nothing -> ffsmImpl xF yF var
                    result -> result

          binaryImpl a1 b1 a2 b2 var = case (ffsmImpl a1 a2 var) of
                    Nothing -> ffsmImpl b1 b2 var
                    result -> result

          checkTermLists (term1:rest1) (term2:rest2) var = case (checkTerms term1 term2 var) of
                    Nothing -> checkTermLists rest1 rest2 var
                    result -> result
          checkTermLists [] [] _ = Nothing
          checkTermLists _ _ _ = Just Nothing

          checkTerms (VarTerm x) y = checkVars x y
          checkTerms (FunctionalTerm name1 terms1) (FunctionalTerm name2 terms2)
                    = if (name1 /= name2) then \_ -> Just Nothing else checkTermLists terms1 terms2 
          checkTerms _ _ = \_ -> Just Nothing 

checkReplEq :: Formula -> Formula -> Var -> Writer [Warning] Bool
checkReplEq f1 f2 var = case findFirstStructureMatching f1 f2 var of
                                Nothing -> return False
                                Just term -> do
                                    r <- replace' f1 var term
                                    return $ isJustTrue $ do {r' <- r; return $ r' == f2 }
             where isJustTrue m = case m of
                                    (Just True) -> True
                                    _ -> False
