module ReadSolidExpressionExample 
( readSolidExpressionExample
) where

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.IO
import Control.Applicative (liftA2)
import Text.Parsec
import Control.Monad.Combinators.Expr

type Parser a = Parsec String () a

atomParser :: Parser (IO Solids.Solid)
atomParser = 
    let filenameParser = char '{' *> (manyTill anyChar (try $ char '}')) <?> "filename"
    in Waterfall.IO.readSolid <$> filenameParser

termParser :: Parser (IO Solids.Solid)
termParser = 
    let brackets = between (char '(') (char ')')
    in brackets exprParser <|> atomParser <?> "term"

exprParser :: Parser (IO Solids.Solid)
exprParser = 
    let binary name f = InfixL  (f <$ name)
        table =
            [ [ binary (char '*') (liftA2 Booleans.intersection)]
              , [ binary (char '+') (liftA2 Booleans.union) 
              , binary (char '-') (liftA2 Booleans.difference)
              ]
            ]
    in makeExprParser termParser table

readSolidExpressionExample :: String -> IO Solids.Solid
readSolidExpressionExample expression = 
    case runParser exprParser () "expression" expression of 
        Left err -> do 
            print err
            fail "Error when parsing expression"
        Right action -> action 


