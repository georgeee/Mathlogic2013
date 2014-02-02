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
                             Nothing -> Left $ DSValidateError [DSFormulaNotProvedError] 
                             _ -> Right $ Proof ds $ extractImpl vs

checkIfTautology f vs = return $ case M.lookup f $ tautologoies vs of
                                Nothing -> False
                                _ -> True
checkIfDsCondition f vs = return $ S.member f $ conditions vs
checkIfIsAxiom f = do { r <- getAxiomId' f ; return $ isJust r }
checkForInferenceRuleImpl a b vs = case M.lookup (Impl a b) $ tautologoies vs of
                                Nothing -> False
                                Just i -> i>=0
checkForInferenceRuleImpl' a b vs x f id = if (checkForInferenceRuleImpl a b vs)
                                                  then if (not $ isFree f x) then return True
                                                       else do tell [InferenceRuleUseWarning id x f]
                                                               return False
                                                  else return False
checkForInferenceRule2 (Impl a (ForAll x b)) vs = checkForInferenceRuleImpl' a b vs x a 2
checkForInferenceRule2 _ _ = return False

checkForInferenceRule3 (Impl (Exists x a) b) vs = checkForInferenceRuleImpl' a b vs x b 3
checkForInferenceRule3 _ _ = return False

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
validateFormula vs (n,f) = let (res, ws) = runWriter $ tryValidators $ map ($vs) $ map ($f) fList
                           in if res
                              then Right $ addFormula vs f
                              else Left $ ValidateError n ws
                               where fList = [checkIfDsCondition, checkIfTautology, checkForInferenceRule2,
                                              checkForInferenceRule3, flip $ const checkIfIsAxiom]
                                     tryValidators [] = return False
                                     tryValidators (w:ws) = do res <- w
                                                               if res then return True
                                                               else tryValidators ws

validateDS (DeductionStatement fs f) = foldM (const validateDSImpl) () fs
    where validateDSImpl f = case findFree f of
                                 Just var -> Left $ DSValidateError $ [DeductionAssumptionWarning var f]
                                 Nothing -> Right $ () 

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
                         

