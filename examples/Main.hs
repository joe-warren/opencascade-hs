module Main (main) where

import CsgExample (csgExample)
import PrismExample (prismExample)
import Waterfall.IO (writeSTL, writeSTEP)
import qualified Waterfall.Solids as Solids
import qualified Options.Applicative as OA
import Control.Applicative ((<|>))
import Control.Monad (join)
outputOption :: OA.Parser (Solids.Solid -> IO ()) 
outputOption =
    let stlOption = writeSTL <$> (OA.option OA.auto (OA.long "resolution" <> OA.help "stl file linear tolerance") <|> pure 0.001) <*>
                        OA.strOption (OA.long "stl" <> OA.metavar "Stl file to write results to")
        stepOption = writeSTEP <$> OA.strOption (OA.long "step" <> OA.metavar "Stl file to write results to")
     in stlOption <|> stepOption

exampleOption :: OA.Parser Solids.Solid
exampleOption = OA.flag' csgExample (OA.long "csg" <> OA.help "example from the wikipedia page on Constructive Solid Geometry" ) <|>
                OA.flag' prismExample (OA.long "prism" <> OA.help "need to give this a better name" )  



main :: IO ()
main = join (OA.execParser opts)
    where
        opts = OA.info ((outputOption <*> exampleOption) OA.<**> OA.helper) 
            (OA.fullDesc
             <> OA.progDesc "generate and write a 3D model"
             <> OA.header "examples for Waterfall-CAD")
