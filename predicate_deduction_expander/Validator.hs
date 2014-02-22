module Validator where
import DataDefinitions
import FormulaReplace
import AxiomSchemes
import Control.Monad
import "mtl" Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

data FormulaSource =   Axiom { axiomId :: Int }
                       | DSCondition
                       | MPConclusion { mpCondition :: Formula, srcFormula  :: Formula }
                       | IR2Conclusion { srcFormula :: Formula }
                       | IR3Conclusion { srcFormula :: Formula }
    deriving (Show, Ord, Eq)
type MPCandidatesMap = M.Map Formula (S.Set Formula)
type TautologoiesMap = M.Map Formula (Int, FormulaSource)
type DSConditionsSet = S.Set Formula

data ValidateState = ValidateState { mpCands :: MPCandidatesMap, 
                                     tautologoies :: TautologoiesMap,
                                     conditions :: DSConditionsSet,
                                     psize :: Int,
                                     aFVars :: DSFreeVarsMap
                                   }
    deriving Show
pairSwap (a,b) = (b,a)
extractProof = impl
    where extractImpl taus dsFormulaP = map snd $ sort $ map pairSwap $ bfsProofTree taus [dsFormulaP] []
          bfsProofTree _ [] res = res
          bfsProofTree taus ((f,(n, src)):q) res = let res' = ((f, n):res)
                                                       throwError :: Formula -> a
                                                       throwError f = error $ "bfsProofTree: formula " ++ (show f) ++ " not found in tautologies"
                                                       tau :: Formula -> (Formula, (Int, FormulaSource))
                                                       tau f = case M.lookup f taus of
                                                                    Nothing -> throwError f
                                                                    Just p@(n, _) -> if n>=0 then (f, p) else throwError f
                                                       bPTForm1 = bfsProofTree taus q res'
                                                       bPTForm2 srcF = bfsProofTree taus ((tau srcF) : q) res'
                                                       bPTForm3 cond srcF = bfsProofTree taus ((tau cond) : (tau srcF) : q) res'
                                                   in case src of
                                                       Axiom _ -> bPTForm1
                                                       DSCondition -> bPTForm1
                                                       MPConclusion cond srcF -> bPTForm3 cond srcF
                                                       IR2Conclusion srcF -> bPTForm2 srcF
                                                       IR3Conclusion srcF -> bPTForm2 srcF
          impl (LinedProof ds@(DeductionStatement dsConds dsFormula) _) vs
                    = case M.lookup dsFormula $ tautologoies vs of
                             Nothing -> Left $ DSValidateError [DSFormulaNotProvedError] 
                             Just dsFormulaP -> Right $ Proof ds $ extractImpl (tautologoies vs) (dsFormula, dsFormulaP)

checkIfTautology f vs = return $ do lookupRes <- M.lookup f $ tautologoies vs
                                    let (_, src) = lookupRes
                                    return src

checkIfDsCondition f vs = return $ if S.member f $ conditions vs then Just DSCondition else Nothing

checkIfIsAxiom f vs = do r <- getAxiomId' (aFVars vs) f
                         return $ do axiomId <- r
                                     return $ Axiom axiomId

checkForInferenceRule2 (Impl a (ForAll x b)) vs = checkForInferenceRuleImpl' a b vs x a 2 IR2Conclusion
checkForInferenceRule2 _ _ = return Nothing 

checkForInferenceRule3 (Impl (Exists x a) b) vs = checkForInferenceRuleImpl' a b vs x b 3 IR3Conclusion
checkForInferenceRule3 _ _ = return Nothing

checkForInferenceRuleImpl' a b vs x f id srcConstructor = do res1 <- check1 a b vs x f id
                                                             res2 <- check2 vs x id
                                                             return $ if res1 && res2
                                                                      then Just $ srcConstructor (a ->- b)
                                                                      else Nothing
                                           where check1 a b vs x f id = if (tautologiesLookup a b vs)
                                                                        then if (not $ isFree f x) then return True
                                                                             else do tell [InferenceRuleVarIsFreeWarning id x f]
                                                                                     return False
                                                                        else return False
                                                 check2 vs x id = if M.member x (aFVars vs)
                                                                  then do tell [InferenceRuleAssumptionVarWarning id x $ (aFVars vs) M.! x]
                                                                          return False
                                                                  else return True
                                                 tautologiesLookup a b vs = case M.lookup (a ->- b) $ tautologoies vs of
                                                                        Nothing -> False
                                                                        Just (i, src) -> i>=0

addFormula state formula src = checkMP (addMP (addToTau state formula src) formula) formula
        where addToTau vs f src = let
                                taus = tautologoies vs
                                n = psize vs
                                vs' = vs { tautologoies = (M.insert f (n, src) taus), psize = n + 1 }
                              in case M.lookup f taus of
                                Nothing -> vs'
                                Just (i,_) -> if i>=0
                                              then vs
                                              else vs'
              addMP vs f@(Impl a b) = let
                                mpCs = mpCands vs
                                taus = tautologoies vs
                                n = psize vs
                            in case M.lookup a taus of
                                Nothing -> vs { mpCands = addToMpCs a b mpCs }
                                Just (i,_) -> if i>=0
                                              then (if M.member b taus then vs else vs { tautologoies = (M.insert b (-1, MPConclusion a f) taus) })
                                              else vs { mpCands = addToMpCs a b mpCs }
              addMP vs _ = vs 
              addToMpCs k v mpCs = case M.lookup k mpCs of
                                    Nothing -> M.insert k (S.singleton v) mpCs
                                    Just s -> M.insert k (S.insert v s) mpCs
              checkMP vs f = let
                                mpCs = mpCands vs
                                taus = tautologoies vs 
                             in case M.lookup f mpCs of
                                    Nothing -> vs
                                    Just s -> foldl (\vs f' -> let taus = tautologoies vs
                                                              in if M.member f' taus
                                                                 then vs
                                                                 else vs{ tautologoies = (M.insert f' (-1, MPConclusion f $ f ->- f') taus) })
                                                    (vs { mpCands = M.delete f mpCs }) $ S.toList s

validateFormula :: ValidateState -> (Int,Formula) -> Either Error ValidateState
validateFormula vs (n,f) = let (res, ws) = runWriter $ tryValidators $ map ($vs) $ map ($f) fList
                           in case res of
                                Just src -> Right $ addFormula vs f src
                                Nothing  -> Left $ ValidateError n ws
                               where fList = [checkIfDsCondition, checkIfTautology, checkForInferenceRule2,
                                              checkForInferenceRule3, checkIfIsAxiom]
                                     tryValidators [] = return Nothing
                                     tryValidators (w:ws) = do res <- w
                                                               case res of
                                                                    r@(Just src) -> return r 
                                                                    Nothing -> tryValidators ws

validateProof :: LinedProof -> Int -> Either Error Proof
validateProof lp expandLevel = case validateImpl lp expandLevel of
                                    (Left err) -> Left err
                                    (Right validateState) -> extractProof lp validateState 
                   where
                        dsCondSet = S.fromList . dsConditions
                        
                        initValidateState ds expandLevel freeVars = ValidateState M.empty M.empty (dsCondSet ds) 0 freeVars 
                        
                        validateImpl :: LinedProof -> Int -> Either Error ValidateState
                        validateImpl (LinedProof ds fs) expandLevel = do
                            let freeVars = findAllFreeVarsInFormulaList $ take expandLevel $ reverse $ dsConditions ds
                            res <- foldM validateFormula (initValidateState ds expandLevel freeVars) fs 
                            return res
                         

