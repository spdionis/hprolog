module HProlog.Parser (
      doParse
    , parseQuery
) where

import HProlog.HProlog
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Text.Parsec.Char

doParse :: String -> IO (Either ParseError Rules)
doParse = parseFromFile rules

parseQuery :: String -> Either ParseError Term
parseQuery = parse function "comment"

rules :: Parser Rules
rules = many clause

clause :: Parser Clause
clause = term >>= \firstTerm -> withoutConditions firstTerm <|> withConditions firstTerm
    where withConditions t = string ":-" >> terms >>= \termList -> schar '.' >> return (t :- termList)
          withoutConditions t = schar '.' >> return (t :- [])

terms :: Parser [Term]
terms = term `sepBy` schar ','

term :: Parser Term
term = variable <|> function

variable :: Parser Term
variable = upperCase >>= \varName -> spaces >> return (Var varName 0)

function :: Parser Term
function = lowerCase >>= \name -> arguments >>= \argList -> return (Func name argList)

atom :: Parser Term
atom = lowerCase >>= \atom -> spaces >> return (Atom atom)

arguments :: Parser [Term]
arguments = schar '(' >> variableOrAtomList >>= \termList -> schar ')' >> return termList
        where variableOrAtomList  = (variable <|> atom) `sepBy` schar ','


lowerCase :: Parser LowerCaseString
lowerCase = lower >>= \x -> many alphaNum >>= \xs -> return (x:xs :: LowerCaseString)

upperCase :: Parser UpperCaseString
upperCase = upper >>= \x -> many alphaNum >>= \xs -> return (x:xs :: UpperCaseString)

schar c = char c >> spaces

special :: Parser Char
special = oneOf ":;+=-*&$#@/.~!" <|> digit