module ReadSolidExpressionExample 
( readSolidExpressionExample
) where

import qualified Waterfall.Solid as Solid
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.IO
import Text.Parsec
import Control.Monad.Combinators.Expr

type Parser a = Parsec String () a

atomParser :: Parser (IO Solid.Solid)
atomParser = 
    let filenameParser = char '{' *> (manyTill anyChar (try $ char '}')) <?> "filename"
    in Waterfall.IO.readSolid <$> filenameParser

termParser :: Parser (IO Solid.Solid)
termParser = 
    let brackets = between (char '(') (char ')')
    in brackets exprParser <|> atomParser <?> "term"

exprParser :: Parser (IO Solid.Solid)
exprParser = 
    let binary name f = InfixL  (f <$ name)
        table =
            [ [ binary (char '*') (liftA2 Booleans.intersection)]
              , [ binary (char '+') (liftA2 Booleans.union) 
              , binary (char '-') (liftA2 Booleans.difference)
              ]
            ]
    in makeExprParser termParser table

readSolidExpressionExample :: String -> IO Solid.Solid
readSolidExpressionExample expression = 
    case runParser exprParser () "expression" expression of 
        Left err -> do 
            print err
            fail "Error when parsing expression"
        Right action -> action 


