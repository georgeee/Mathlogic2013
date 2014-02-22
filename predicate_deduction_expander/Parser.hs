module Parser{-(readDeductionStatement, readTerm, readFormula, readProof) -}where

import DataDefinitions
import Control.Applicative((<*))
import Data.Text(splitOn,pack,unpack, strip)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity

type SingleBinarySimpleOpLevelImpl s u m a = s -> (a -> a -> a) -> Assoc -> [Operator s u m a]
type SingleBinarySimpleOpLevel a = SingleBinarySimpleOpLevelImpl String () Identity a
type SingleUnarySimpleOpLevelImpl s u m a = s -> (a -> a) -> [Operator s u m a]
type SingleUnarySimpleOpLevel a = SingleUnarySimpleOpLevelImpl String () Identity a
type MultiPrefixLevelImpl s u m a = [ParsecT s u m (a -> a)] -> [Operator s u m a]
type MultiPrefixLevel a = MultiPrefixLevelImpl String () Identity a

binarySimpleOpLevel :: SingleBinarySimpleOpLevel a
binarySimpleOpLevel  = \name fun assoc -> [Infix (createSimpleOpParser name fun) assoc]

prefixSimpleOpLevel :: SingleUnarySimpleOpLevel a
prefixSimpleOpLevel  = \name fun -> [Prefix  . chainl1 (createSimpleOpParser name fun) $ return (.)]

prefixMultiLevel :: MultiPrefixLevel a
prefixMultiLevel = \fs -> [Prefix  . chainl1 (choice fs) $ return (.)]

postfixSimpleOpLevel :: SingleUnarySimpleOpLevel a
postfixSimpleOpLevel = \name fun -> [Postfix . chainl1 (createSimpleOpParser name fun) $ return (flip (.))]

createSimpleOpParser name fun = skipSpaces >> string name >> skipSpaces >> return fun

createQuantorParser name fun = do skipSpaces
                                  string name
                                  skipSpaces
                                  var <- parseVar
                                  skipSpaces
                                  return $ fun var

nameContents = many (digit <|> char '_')
parentheses :: Parser a -> Parser a
parentheses f = do skipSpaces
                   char '('
                   t <- f
                   skipSpaces
                   char ')'
                   return t
skipSpaces = many space

parseVar :: Parser Var
parseVar = do skipSpaces
              c <- lower
              cs <- nameContents 
              return (Var (c:cs))
    <?> "identifier"

termName :: Parser String
termName = do skipSpaces
              c <- lower
              cs <- nameContents
              return (c:cs)
    <?> "name of term"

termList :: Parser [Term]
termList = do skipSpaces
              char '('
              skipSpaces
              first <- parseTerm
              rest <- many(do{skipSpaces; char ',' ; parseTerm})
              skipSpaces
              char ')'
              return (first:rest)
    <?> "list of terms"

parseTerm' :: Parser Term
parseTerm' = do skipSpaces
                res <- form2 <|> (try form3) <|> form1 <|> form4
                skipSpaces
                return res
         <?> "term"
        where
            form1 = do var <- parseVar
                       return (VarTerm var)
            form2 = do parentheses parseTerm
            form3 = do name <- termName
                       subterms <- termList
                       return (FunctionalTerm name subterms)
            form4 = do char '0'
                       return (FunctionalTerm "0" [])

parseTerm = buildExpressionParser table parseTerm'
         where
            table = [ postfixSimpleOpLevel "'" (\x -> FunctionalTerm "'" [x]),
                      binarySimpleOpLevel "*" (\x -> \y -> FunctionalTerm "*" [x,y]) AssocLeft,
                      binarySimpleOpLevel "+" (\x -> \y -> FunctionalTerm "+" [x,y]) AssocLeft ]

testParseTerm str = parse parseTerm "" str 

predicateName :: Parser String
predicateName = do skipSpaces
                   c <- upper
                   cs <- nameContents
                   return (c:cs)
                <?> "name of predicate"

parseAtomicFormula :: Parser Formula
parseAtomicFormula = do skipSpaces
                        parseResult <- (try (parentheses parseFormula))
                                       <|> (try parseEqualsPredicate)
                                       <|> (try parsePredicate)
                                       <|> parseEmptyPredicate
                        skipSpaces
                        return parseResult
        <?> "atomic formula"
        where
            parseEmptyPredicate = do
                name <- predicateName
                return (Predicate name [])
            parsePredicate = do
                name <- predicateName
                terms <- termList
                return (Predicate name terms)
            parseEqualsPredicate = do
                term1 <- parseTerm
                skipSpaces
                char '='
                skipSpaces
                term2 <- parseTerm
                return (Predicate "=" [term1, term2])


parseFormula = do skipSpaces
                  buildExpressionParser table parseAtomicFormula
         where
            table = [ prefixMultiLevel [createSimpleOpParser "!" Not, createQuantorParser "?" Exists, createQuantorParser "@" ForAll],
                      binarySimpleOpLevel "&" And AssocLeft,
                      binarySimpleOpLevel "|" Or AssocLeft, binarySimpleOpLevel "->" Impl AssocRight
                       ]

testParseFormula str = parse parseFormula "" str


readDeductionStatement :: String -> Either Error DeductionStatement
readDeductionStatement = parseImpl . (map unpack) . (splitOn $ pack "|-") . pack
    where parseImpl (fsStr:fStr:[]) =
                case (parse (parseFormula `sepBy` (char ',')) "left part of deduction statement" fsStr) of
                    (Left msg) -> Left $ ParseError $ show msg
                    (Right formulas) -> case (parse parseFormula "right part of deduction statement" fStr) of
                                        (Left msg) -> Left $ ParseError $ ("In '" ++ fStr ++ "'") ++ (show msg)
                                        (Right formula) -> Right $ DeductionStatement formulas formula 
          parseImpl _ = Left $ ParseError "too many |- delimeters"

readSimple :: Parser a -> String -> Either Error a
readSimple f str = case (parse f "" str) of
                (Left msg) -> Left $ ParseError $ show msg
                (Right result) -> Right result

readTerm = readSimple parseTerm 
readFormula = readSimple parseFormula 


readFormulaList :: [String] -> Either Error [(Int,Formula)]
readFormulaList fs = rfImpl fs 0 []
        where
            rfImpl [] n res = Right $ reverse res
            rfImpl ([]:ss) n res = rfImpl ss (n+1) res
            rfImpl (s:ss) n fList = case (readFormula s) of
                                        (Left (ParseError msg)) -> Left $ ParseError $ msg ++ " at line #" ++ (show $ n + 1)
                                        (Right formula) -> rfImpl ss (n+1) ((n,formula):fList)

readProof :: String -> Either Error LinedProof
readProof str = rpImpl $ map (unpack . strip . pack) $ lines str
        where rpImpl (first:rest) = do ds <- readDeductionStatement first
                                       fs <- readFormulaList rest
                                       return $ LinedProof ds fs 
