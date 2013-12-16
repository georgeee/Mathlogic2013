module PredicateParser(readDeductionStatement, readTerm, readFormula) where

import DataDefinitions
import Control.Applicative((<*))
import Data.Text(splitOn,pack,unpack)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

nameContents = many (letter <|> digit <|> char '_')
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
              c <- lower <|> char '_'
              cs <- nameContents 
              return (Var (c:cs))
    <?> "identifier"

termName :: Parser String
termName = do skipSpaces
              c <- lower <|> char '_'
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

parseTerm :: Parser Term
parseTerm = do { skipSpaces ; form2 <|> (try form3) <|> form1 } <?> "term"
        where
            form1 = do var <- parseVar
                       return (VarTerm var)
            form2 = do parentheses parseTerm
            form3 = do name <- termName
                       subterms <- termList
                       return (FunctionalTerm name subterms)

testParseTerm str = parse parseTerm "" str 

predicateName :: Parser String
predicateName = do skipSpaces
                   c <- upper <|> char '_'
                   cs <- nameContents
                   return (c:cs)
                <?> "name of predicate"

parseAtomicFormula :: Parser Formula
parseAtomicFormula = do
                        parseResult <- (try (parentheses parseFormula))
                                       <|> (try parsePredicate)
                                       <|> parseEmptyPredicate
                                       <|> parseExists
                                       <|> parseForAll
                        skipSpaces
                        return parseResult
        <?> "formula"
        where
            parseEmptyPredicate = do
                name <- predicateName
                return (Predicate name [])
            parsePredicate = do
                name <- predicateName
                terms <- termList
                return (Predicate name terms)
            parseExists = parseWithQuantor '?' Exists
            parseForAll = parseWithQuantor '@' ForAll
            parseWithQuantor ch constructor = do
                skipSpaces
                char ch
                var <- parseVar
                formula <- parseFormula
                return (constructor var formula)

parseFormula = buildExpressionParser table parseAtomicFormula
         where
            table = [ [prefix "!" Not], [binary "&" And AssocLeft],
                      [binary "|" Or AssocLeft], [binary "->" Impl AssocLeft] ]
            binary name fun assoc = Infix (string name >> return fun) assoc
            prefix name fun       = Prefix (string name >> return fun)

testParseFormula str = parse parseFormula "" str


readDeductionStatement :: String -> Either ErrorMsg DeductionStatement
readDeductionStatement = parseImpl . (map unpack) . (splitOn $ pack "|-") . pack
    where parseImpl (fsStr:fStr:[]) =
                case (parse (parseFormula `sepBy` (char ',')) "left" fsStr) of
                    (Left msg) -> Left $ show msg
                    (Right formulas) -> case (parse parseFormula "right" fStr) of
                                        (Left msg) -> Left $ show msg
                                        (Right formula) -> Right $ DeductionStatement formulas formula 
          parseImpl _ = Left "too many |- delimeters"

readSimple :: Parser a -> String -> Either ErrorMsg a
readSimple f str = case (parse f "" str) of
                (Left msg) -> Left $ show msg
                (Right result) -> Right result

readTerm = readSimple parseTerm 
readFormula = readSimple parseFormula 

