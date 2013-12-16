module Processor where
import DataDefinitions
import PredicateParser 
import Data.Text(unpack,pack,strip)


readProof :: String -> Either ErrorMsg Proof
readProof str = rpImpl $ filter null $ map (unpack . strip . pack) $ lines str
        where rpImpl (first:rest) = case (readDeductionStatement first) of
                    (Left msg) -> Left msg
                    (Right ds) -> case fList of
                                    (Left msg) -> Left msg
                                    (Right fs) -> Right $ Proof ds fs
              fListImpl = map readFormula
              fList = 
