module Main where
import ErrorShowInstancesRussian
import Validator 
import DataDefinitions
import Parser 
import DeductionExpander
import System.Environment

import FormulaReplace
import "mtl" Control.Monad.Writer
import AxiomSchemes
import ArithmeticAxioms


data MainConfig = MainConfig { inFile :: String,
                               outFile :: String,
                               checkMode :: Bool,
                               expandLevel :: Int
                             }

createConfig :: [String] -> MainConfig
createConfig = impl $ MainConfig "" "" False 1
               where impl conf [] = conf
                     impl conf ("-c":rest) = impl conf{checkMode=True} rest
                     impl conf ("-e":eL:rest) = let n = read eL in impl conf{expandLevel=(if n<0 then 0 else n)} rest
                     impl conf (inF:outF:rest) = impl conf{inFile=inF, outFile=outF} rest
                     impl conf (inF:rest) = impl conf{inFile=inF} rest

readValidateProof str expandLevel = do
    lp <- readProof str
    p <- validateProof lp expandLevel
    return p

readValidateTryExpand str expandLevel = do
    p <- readValidateProof str expandLevel
    return $ tryExpandDeduction p expandLevel

processContent config content = case readValidateTryExpand content $ expandLevel config of
        (Left err) -> show err
        (Right proof) -> if (checkMode config)
                         then "Доказательство корректно."
                         else show proof

processMain' config = do
    content <- readFile $ inFile config
    let  ls = lines content
         content' = if checkMode config
                    then "|-" ++ (last ls) ++ "\n" ++ content
                    else content
         outF = outFile config
         res = processContent config content'
     in  case outF of
           [] -> putStrLn res
           outF -> writeFile outF $ res

processMain = processMain' . createConfig

main = do
    args <- getArgs
    processMain args 
