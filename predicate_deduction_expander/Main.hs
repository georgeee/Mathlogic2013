module Main where
import ErrorShowInstancesDefault
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

processContent content = case readValidateTryExpand content of
        (Left err) -> show err
        (Right proof) -> show proof

processMain inFile args = do
    content <- readFile inFile
    processMainImpl args content
        where processMainImpl (outFile:as) content = writeFile outFile $ processContent content
              processMainImpl [] content = putStrLn $ processContent content

main = do
    args <- getArgs
    let (inFile:rest) = args in processMain inFile rest 
