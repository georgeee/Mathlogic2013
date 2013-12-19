module Validator(validateProof) where
import DataDefinitions
import PredicateParser 
import FormulaReplace
import AxiomSchemes
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

type MPCandidatesMap = M.Map Formula (S.Set Formula)
type TautologoiesMap = M.Map Formula Int
type DSConditionsSet = S.Set Formula

data ValidateState = ValidateState { mpCandidates :: MPCandidatesMap, 
                                     tautologoies :: TautologoiesMap,
                                     conditions :: DSConditionsSet,
                                     psize :: Int }
    deriving Show
pairSwap (a,b) = (b,a)
extractProof = impl
    where extractImpl vs = map snd $ sort $ map pairSwap $ filter ((>=0).snd) $ M.toList $ tautologoies vs
          impl (LinedProof ds@(DeductionStatement dsConds dsFormula) _) vs
                    = case M.lookup dsFormula $ tautologoies vs of
                             Nothing -> Left $ VError "Formula from deduction statement wasn't proved"
                             _ -> Right $ Proof ds $ extractImpl vs

checkIfTautology f vs = case M.lookup f $ tautologoies vs of
                                Nothing -> False
                                _ -> True
checkIfDsCondition f vs = S.member f $ conditions vs
checkIfIsAxiom f = case isAxiom f of
                                Nothing -> False
                                _ -> True
checkForInferenceRuleImpl a b vs = case M.lookup (Impl a b) $ tautologoies vs of
                                Nothing -> False
                                Just i -> i>=0
checkForInferenceRule2 (Impl a (ForAll x b)) vs = (checkForInferenceRuleImpl a b vs) && (not $ isFree a x)
checkForInferenceRule2 _ _ = False

checkForInferenceRule3 (Impl (Exists x a) b) vs = (checkForInferenceRuleImpl a b vs) && (not $ isFree b x)
checkForInferenceRule3 _ _ = False

determineError state n formula = NumberedVError n $ "state" ++(show state) ++ " formula:" ++ (show formula)

addFormula state formula = checkMP (addMP (addToTau state formula) formula) formula
        where addToTau vs@(ValidateState mpCs taus cs n) f = case M.lookup f taus of
                                Nothing -> ValidateState mpCs (M.insert f n taus) cs (n+1)
                                Just i -> if i>=0
                                            then vs
                                            else ValidateState mpCs (M.insert f n taus) cs (n+1)
              addMP vs@(ValidateState mpCs taus cs n) f@(Impl a b) = case M.lookup a taus of
                                Nothing -> ValidateState (addToMpCs a b mpCs) taus cs n
                                Just i -> if i>=0
                                            then ValidateState mpCs (M.insert b (-1) taus) cs n 
                                            else ValidateState (addToMpCs a b mpCs) taus cs n
              addMP vs _ = vs 
              addToMpCs k v mpCs = case M.lookup k mpCs of
                                    Nothing -> M.insert k (S.singleton v) mpCs
                                    Just s -> M.insert k (S.insert v s) mpCs
              checkMP vs@(ValidateState mpCs taus cs n) f = case M.lookup f mpCs of
                                    Nothing -> vs
                                    Just s -> foldl addToTau (ValidateState (M.delete f mpCs) taus cs n) $ S.toList s

validateFormula :: ValidateState -> (Int,Formula) -> Either Error ValidateState
validateFormula vs (n,f) = if any ($vs) $ map ($f) fList
                           then Right $ addFormula vs f
                           else Left $ determineError vs n f
                               where fList = [checkIfDsCondition, checkIfTautology, checkForInferenceRule2,
                                              checkForInferenceRule3, flip $ const checkIfIsAxiom]

validateDS (DeductionStatement fs f) = foldM (const validateDSImpl) () fs
    where validateDSImpl f = if hasFree f
                             then Left $ VError $ "Deduction condition " ++ (show f) ++ " has free variable(s)"
                             else Right $ () 

validateProof :: LinedProof -> Either Error Proof
validateProof lp = case validateImpl lp of
                        (Left err) -> Left err
                        (Right validateState) -> extractProof lp validateState 
                   where
                        dsCondSet = S.fromList . dsConditions
                        initVaidateState ds = ValidateState M.empty M.empty (dsCondSet ds) 0
                        validateImpl :: LinedProof -> Either Error ValidateState
                        validateImpl (LinedProof ds fs) = do
                            validateDS ds
                            res <- foldM validateFormula (initVaidateState ds) fs 
                            return res
                         

