module Main where
import ErrorShowInstancesRussian
import Validator 
import DataDefinitions
import Parser 
import DeductionExpander
import System.Environment

data MainConfig = MainConfig { inFile :: String,
                               outFile :: String,
                               checkOnly :: Bool }

createConfig :: [String] -> MainConfig
createConfig = impl $ MainConfig "" "" False
               where impl conf [] = conf
                     impl conf ("-c":rest) = impl conf{checkOnly=True} rest
                     impl conf (inF:outF:rest) = impl conf{inFile=inF, outFile=outF} rest
                     impl conf (inF:rest) = impl conf{inFile=inF} rest

readValidateProof str = do
    lp <- readProof str
    p <- validateProof lp
    return p

readValidateTryExpand str = do
    p <- readValidateProof str
    return $ tryExpandDeduction p

processContent config content = case readValidateTryExpand content of
        (Left err) -> show err
        (Right proof) -> if (checkOnly config)
                         then "Доказательство корректно."
                         else show proof

processMain' config = do
    content <- readFile $ inFile config
    let  ls = lines content
         content' = if checkOnly config
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
