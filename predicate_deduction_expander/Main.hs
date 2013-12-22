module Main where
import Validator 
import DataDefinitions
import Parser 
import DeductionExpander
import System.Environment

readValidateProof str = do
    lp <- readProof str
    p <- validateProof lp
    return p

readValidateTryExpand str = do
    p <- readValidateProof str
    return $ tryExpandDeduction p

processMain inFile outFile = do
    content <- readFile inFile
    writeFile outFile (case readValidateTryExpand content of
        (Left err) -> show err
        (Right proof) -> show proof)

main = do
    args <- getArgs
    processMain (args !! 0) (args !! 1) 
