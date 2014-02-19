module DeductionExpander(expandDeduction, tryExpandDeduction) where
import DataDefinitions
import Data.List
import FormulaReplace
import AxiomSchemes
import qualified Data.Map as M
import qualified Data.Set as S
import "mtl" Control.Monad.State

type FormulaSet = S.Set Formula
type FormulaMap = M.Map Formula Int
type MPConclusionsMap = M.Map Formula Formula
type MPCandidatesMap = M.Map Formula MPConclusionsMap
data PermanentConds = PermanentConds {pcDSConds :: FormulaSet,
                                      expandee :: Formula,
                                      pcDS :: DeductionStatement}
    deriving Show
data ExpandStateData = ExpandStateData {permConds :: PermanentConds,
                                        fMap :: FormulaMap,
                                        mpCandidates :: MPCandidatesMap, 
                                        mpConclusions :: MPConclusionsMap,
                                        origTautologies :: FormulaSet 
                                       }
    deriving Show
type ExpandState = State ExpandStateData

composeResult state = Proof (pcDS $ permConds state) $ toFList state
    where toFList state = let map' = fMap state
                              statement = dsFormula $ pcDS $ permConds state
                          in map snd $ sort $ map swap $ M.toList $ M.filter (<= (map' M.! statement)) map'
          swap (a,b) = (b,a)

createState (Proof (DeductionStatement conds f) fs)
      = let dsConds = init conds
            expandee = last conds
            dsCondsSet = S.fromList dsConds
            ds = DeductionStatement dsConds (Impl expandee f)
            pc = PermanentConds dsCondsSet expandee ds
        in  ExpandStateData pc M.empty M.empty M.empty S.empty

addFormula f = do state <- get
                  let map' = fMap state
                      in if M.member f map' then return ()
                                            else put $ state {fMap = M.insert f (M.size map') map'}
getExpandee :: ExpandState Formula
getExpandee = do state <- get
                 return $ expandee $ permConds state

--------------------------------------------------------

addDSConditionProof ci = do a <- getExpandee
                            addFormula $ ci ->- a ->- ci
                            addFormula $ ci
                            addFormula $ a ->- ci
addSelfProof           = do a <- getExpandee
                            addFormula $ (a ->- (a ->- a)) ->- (a ->- (a ->- a) ->- a) ->- (a ->- a)
                            addFormula $ a ->- a ->- a
                            addFormula $ (a ->- (a ->- a) ->- a) ->- (a ->- a)
                            addFormula $ (a ->- (a ->- a) ->- a)
                            addFormula $ a ->- a 
addMPProof ci cj       = do a <- getExpandee
                            addFormula $ (a ->- cj) ->- (a ->- cj ->- ci) ->- (a ->- ci)
                            addFormula $ a ->- cj
                            addFormula $ (a ->- cj ->- ci) ->- (a ->- ci)
                            addFormula $ a ->- cj ->- ci
                            addFormula $ a ->- ci

------------------------------------------------------------
expandDeduction (Proof (DeductionStatement [] _) _) = Nothing
expandDeduction proof = let (_, state) = runState (addAllFormulas $ fList proof) $ createState proof
                        in Just $ composeResult state
tryExpandDeduction proof = case expandDeduction proof of
                            Nothing -> proof
                            Just proof' -> proof'

addAllFormulas [] = return ()
addAllFormulas (f:fs) = do addFormulaProof f
                           addAllFormulas fs

addFormulaProof ci = do a <- getExpandee
                        if a == ci then addSelfProof
                        else do
                           isCond <- isDSCondition ci
                           if isCond || (isAxiom M.empty ci)
                           then addDSConditionProof ci
                           else do
                              mpPred <- getMPPred ci
                              case mpPred of
                                Just cj -> addMPProof ci cj
                                Nothing -> do
                                        isIR2 <- isIR2Conclusion ci
                                        if isIR2 then addIR2Proof ci
                                        else do
                                            isIR3 <- isIR3Conclusion ci
                                            if isIR3 then addIR3Proof ci
                                            else error "Input proof is invalid" --it shouldn't happen, cause proof is being validated before deduction expand process
                        processMPHandling ci
                        state <- get
                        put state{origTautologies = S.insert ci $ origTautologies state}
                                       
processMPHandling ci = do state <- get
                          let cands = mpCandidates state
                              concls = mpConclusions state
                            in case M.lookup ci cands of
                                Nothing -> return ()
                                Just ciConcls -> put state{mpConclusions = M.union concls ciConcls,
                                                           mpCandidates = M.delete ci cands}
                          case ci of
                            (Impl a b) -> if S.member a $ origTautologies state then
                                            let concls = mpConclusions state in
                                                if M.notMember b concls then
                                                    put state{mpConclusions = M.insert b a concls}
                                                else return ()
                                          else let cands = mpCandidates state
                                                   insert = \concls -> put state{mpCandidates = M.insert a concls cands}
                                            in case M.lookup a cands of
                                                Nothing -> insert (M.singleton b a)
                                                Just concls -> insert (M.insert b a concls)
                            _ -> return ()
                            
getMPPred ci = do state <- get
                  return $ M.lookup ci $ mpConclusions state
isDSCondition ci = do state <- get
                      return $ S.member ci $ pcDSConds $ permConds state

addIR2Proof (Impl b q@(ForAll x c)) = do a <- getExpandee
                                         addProof2Internal a b c
                                         addProof1Internal a b q
                 
addIR3Proof (Impl q@(Exists x b) c) = do a <- getExpandee
                                         addProof3Internal a b c
                                         addProof3Internal a q c

isIRConclusionImpl a b isFree = do state <- get
                                   let b1 = S.member (Impl a b) $ origTautologies state
                                       b2 = not isFree 
                                    in return $ b1 && b2

isIR2Conclusion (Impl a (ForAll x b)) = isIRConclusionImpl a b $ isFree a x
isIR2Conclusion _ = return False

isIR3Conclusion (Impl (Exists x a) b) = isIRConclusionImpl a b $ isFree b x
isIR3Conclusion _ = return False

--- ((a -&- b) ->- c) |- (a ->- (b ->- c))
addProof1Internal a b c = do
         addFormula $ (a ->- (b ->- (a -&- b)))
         addFormula $ ((a -&- b) ->- c)
         addFormula $ (((a -&- b) ->- c) ->- (a ->- ((a -&- b) ->- c)))
         addFormula $ (a ->- ((a -&- b) ->- c))
         addFormula $ (((a -&- b) ->- c) ->- (b ->- ((a -&- b) ->- c)))
         addFormula $ ((((a -&- b) ->- c) ->- (b ->- ((a -&- b) ->- c))) ->- (a ->- (((a -&- b) ->- c) ->- (b ->- ((a -&- b) ->- c)))))
         addFormula $ (a ->- (((a -&- b) ->- c) ->- (b ->- ((a -&- b) ->- c))))
         addFormula $ ((a ->- ((a -&- b) ->- c)) ->- ((a ->- (((a -&- b) ->- c) ->- (b ->- ((a -&- b) ->- c)))) ->- (a ->- (b ->- ((a -&- b) ->- c)))))
         addFormula $ ((a ->- (((a -&- b) ->- c) ->- (b ->- ((a -&- b) ->- c)))) ->- (a ->- (b ->- ((a -&- b) ->- c))))
         addFormula $ (a ->- (b ->- ((a -&- b) ->- c)))
         addFormula $ ((b ->- (a -&- b)) ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c)))
         addFormula $ (((b ->- (a -&- b)) ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c))) ->- (a ->- ((b ->- (a -&- b)) ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c)))))
         addFormula $ (a ->- ((b ->- (a -&- b)) ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c))))
         addFormula $ ((a ->- (b ->- (a -&- b))) ->- ((a ->- ((b ->- (a -&- b)) ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c)))) ->- (a ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c)))))
         addFormula $ ((a ->- ((b ->- (a -&- b)) ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c)))) ->- (a ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c))))
         addFormula $ (a ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c)))
         addFormula $ ((a ->- (b ->- ((a -&- b) ->- c))) ->- ((a ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c))) ->- (a ->- (b ->- c))))
         addFormula $ ((a ->- ((b ->- ((a -&- b) ->- c)) ->- (b ->- c))) ->- (a ->- (b ->- c)))
         addFormula $ (a ->- (b ->- c))

--- (a ->- (b ->- c)) |- ((a -&- b) ->- c)
addProof2Internal a b c = do
         addFormula $ ((a -&- b) ->- b)
         addFormula $ ((a -&- b) ->- a)
         addFormula $ (a ->- (b ->- c))
         addFormula $ ((a ->- (b ->- c)) ->- ((a -&- b) ->- (a ->- (b ->- c))))
         addFormula $ ((a -&- b) ->- (a ->- (b ->- c)))
         addFormula $ (((a -&- b) ->- a) ->- (((a -&- b) ->- (a ->- (b ->- c))) ->- ((a -&- b) ->- (b ->- c))))
         addFormula $ (((a -&- b) ->- (a ->- (b ->- c))) ->- ((a -&- b) ->- (b ->- c)))
         addFormula $ ((a -&- b) ->- (b ->- c))
         addFormula $ (((a -&- b) ->- b) ->- (((a -&- b) ->- (b ->- c)) ->- ((a -&- b) ->- c)))
         addFormula $ (((a -&- b) ->- (b ->- c)) ->- ((a -&- b) ->- c))
         addFormula $ ((a -&- b) ->- c)

--- (a ->- (b ->- c)) |- (b ->- (a ->- c))
addProof3Internal a b c = do
         addFormula $ (b ->- (a ->- b))
         addFormula $ (a ->- (b ->- c))
         addFormula $ ((a ->- (b ->- c)) ->- (b ->- (a ->- (b ->- c))))
         addFormula $ (b ->- (a ->- (b ->- c)))
         addFormula $ ((a ->- b) ->- ((a ->- (b ->- c)) ->- (a ->- c)))
         addFormula $ (((a ->- b) ->- ((a ->- (b ->- c)) ->- (a ->- c))) ->- (b ->- ((a ->- b) ->- ((a ->- (b ->- c)) ->- (a ->- c)))))
         addFormula $ (b ->- ((a ->- b) ->- ((a ->- (b ->- c)) ->- (a ->- c))))
         addFormula $ ((b ->- (a ->- b)) ->- ((b ->- ((a ->- b) ->- ((a ->- (b ->- c)) ->- (a ->- c)))) ->- (b ->- ((a ->- (b ->- c)) ->- (a ->- c)))))
         addFormula $ ((b ->- ((a ->- b) ->- ((a ->- (b ->- c)) ->- (a ->- c)))) ->- (b ->- ((a ->- (b ->- c)) ->- (a ->- c))))
         addFormula $ (b ->- ((a ->- (b ->- c)) ->- (a ->- c)))
         addFormula $ ((b ->- (a ->- (b ->- c))) ->- ((b ->- ((a ->- (b ->- c)) ->- (a ->- c))) ->- (b ->- (a ->- c))))
         addFormula $ ((b ->- ((a ->- (b ->- c)) ->- (a ->- c))) ->- (b ->- (a ->- c)))
         addFormula $ (b ->- (a ->- c))
