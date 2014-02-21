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

type MPCandidatesMap = M.Map Formula (S.Set Formula)
type TautologoiesMap = M.Map Formula Int
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
    where extractImpl vs = map snd $ sort $ map pairSwap $ filter ((>=0).snd) $ M.toList $ tautologoies vs
          impl (LinedProof ds@(DeductionStatement dsConds dsFormula) _) vs
                    = case M.lookup dsFormula $ tautologoies vs of
                             Nothing -> Left $ DSValidateError [DSFormulaNotProvedError] 
                             _ -> Right $ Proof ds $ extractImpl vs

checkIfTautology f vs = return $ M.member f $ tautologoies vs
checkIfDsCondition f vs = return $ S.member f $ conditions vs
checkIfIsAxiom f vs = do { r <- getAxiomId' (aFVars vs) f ; return $ isJust r }

checkForInferenceRule2 (Impl a (ForAll x b)) vs = checkForInferenceRuleImpl' a b vs x a 2
checkForInferenceRule2 _ _ = return False

checkForInferenceRule3 (Impl (Exists x a) b) vs = checkForInferenceRuleImpl' a b vs x b 3
checkForInferenceRule3 _ _ = return False

checkForInferenceRuleImpl' a b vs x f id = do res1 <- check1 a b vs x f id
                                              res2 <- check2 vs x id
                                              return $ res1 && res2
                                           where check1 a b vs x f id = if (tautologiesLookup a b vs)
                                                                        then if (not $ isFree f x) then return True
                                                                             else do tell [InferenceRuleVarIsFreeWarning id x f]
                                                                                     return False
                                                                        else return False
                                                 check2 vs x id = if M.member x (aFVars vs)
                                                                  then do tell [InferenceRuleAssumptionVarWarning id x $ (aFVars vs) M.! x]
                                                                          return False
                                                                  else return True
                                                 tautologiesLookup a b vs = case M.lookup (Impl a b) $ tautologoies vs of
                                                                        Nothing -> False
                                                                        Just i -> i>=0

addFormula state formula = checkMP (addMP (addToTau state formula) formula) formula
        where addToTau vs f = let
                                taus = tautologoies vs
                                n = psize vs
                                vs' = vs { tautologoies = (M.insert f n taus), psize = n + 1 }
                              in case M.lookup f taus of
                                Nothing -> vs'
                                Just i -> if i>=0
                                            then vs
                                            else vs'
              addMP vs f@(Impl a b) = let
                                mpCs = mpCands vs
                                taus = tautologoies vs
                                n = psize vs
                            in case M.lookup a taus of
                                Nothing -> vs { mpCands = addToMpCs a b mpCs }
                                Just i -> if i>=0
                                          then (if M.member b taus then vs else vs { tautologoies = (M.insert b (-1) taus) })
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
                                    Just s -> foldl (\vs f -> let taus = tautologoies vs
                                                              in if M.member f taus
                                                                 then vs
                                                                 else vs{ tautologoies = (M.insert f (-1) taus) })
                                                    (vs { mpCands = M.delete f mpCs }) $ S.toList s

validateFormula :: ValidateState -> (Int,Formula) -> Either Error ValidateState
validateFormula vs (n,f) = let (res, ws) = runWriter $ tryValidators $ map ($vs) $ map ($f) fList
                           in if res
                              then Right $ addFormula vs f
                              else Left $ ValidateError n ws
                               where fList = [checkIfDsCondition, checkIfTautology, checkForInferenceRule2,
                                              checkForInferenceRule3, checkIfIsAxiom]
                                     tryValidators [] = return False
                                     tryValidators (w:ws) = do res <- w
                                                               if res then return True
                                                               else tryValidators ws

validateProof :: LinedProof -> Int -> Either Error Proof
validateProof lp expandLevel = case validateImpl lp expandLevel of
                                    (Left err) -> Left err
                                    (Right validateState) -> extractProof lp validateState 
                   where
                        dsCondSet = S.fromList . dsConditions
                        
                        initValidateState ds expandLevel freeVars = ValidateState M.empty M.empty (dsCondSet ds) 0 freeVars 
                        
                        validateImpl :: LinedProof -> Int -> Either Error ValidateState
                        validateImpl (LinedProof ds fs) expandLevel = do
                            freeVars <- findAllFreeVarsInFormulaList $ take expandLevel $ reverse $ dsConditions ds
                            res <- foldM validateFormula (initValidateState ds expandLevel freeVars) fs 
                            return res
                         

