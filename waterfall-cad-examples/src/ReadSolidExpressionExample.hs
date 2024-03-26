module ReadSolidExpressionExample 
( readSolidExpressionExample
) where

import qualified Waterfall.Solids as Solids
import qualified Waterfall.Booleans as Booleans
import qualified Waterfall.IO
import Control.Applicative (liftA2)
import Text.Parsec
import Control.Monad.Combinators.Expr
import Waterfall.Internal.Solid (debug)

type Parser a = Parsec String () a

readS :: FilePath -> IO Solids.Solid
readS file = do
    f <- Waterfall.IO.readSolid file
    putStrLn file
    putStrLn . debug $ f
    return f

 
atomParser :: Parser (IO Solids.Solid)
atomParser = 
    let filenameParser = char '{' *> (manyTill anyChar (try $ char '}')) <?> "filename"
    in readS <$> filenameParser

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


