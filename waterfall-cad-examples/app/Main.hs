module Main (main) where

import CsgExample (csgExample)
import PrismExample (prismExample)
import GearExample (gearExample)
import FilletExample (filletExample)
import RevolutionExample (revolutionExample)
import SweepExample (sweepExample)
import OffsetExample (offsetExample)
import LoftExample (loftExample)
import TextExample (textExample)
import BoundingBoxExample (boundingBoxExample)
import ReadSolidExpressionExample (readSolidExpressionExample)
import SVG.PathExample (pathExample)
import Waterfall.IO (writeSTL, writeSTEP, writeGLTF, writeGLB, writeOBJ)
import qualified Waterfall.Solids as Solids
import qualified Options.Applicative as OA
import Control.Applicative ((<|>), liftA2)
import Control.Monad (join)

outputOption :: OA.Parser (Solids.Solid -> IO ()) 
outputOption =
    let stlOption = (flip writeSTL) <$> OA.strOption (OA.long "stl" <> OA.metavar "Stl file to write results to")
        gltfOption = (flip writeGLTF) <$> OA.strOption (OA.long "gltf" <> OA.metavar "GLTF file to write results to")
        glbOption = (flip writeGLB) <$> OA.strOption (OA.long "glb" <> OA.metavar "GLB file to write results to")
        objOption = (flip writeOBJ) <$> OA.strOption (OA.long "obj" <> OA.metavar "OBJ file to write results to")
        meshOptionsNoResolution = stlOption <|> gltfOption <|> glbOption <|> objOption
        meshOptions = meshOptionsNoResolution <*>
            (OA.option OA.auto (OA.long "resolution" <> OA.help "linear tolerance for mesh file formats") <|> pure 0.001)
        stepOption = writeSTEP <$> OA.strOption (OA.long "step" <> OA.metavar "Stl file to write results to")
     in meshOptions <|> stepOption

exampleOption :: OA.Parser (IO Solids.Solid)
exampleOption = 
    fmap pure (
      OA.flag' csgExample (OA.long "csg" <> OA.help "example from the wikipedia page on Constructive Solid Geometry" ) <|>
      OA.flag' prismExample (OA.long "prism" <> OA.help "need to give this a better name" ) <|>
      OA.flag' filletExample (OA.long "fillet" <> OA.help "demonstrates adding fillets to an object" ) <|>
      OA.flag' revolutionExample (OA.long "revolution" <> OA.help "demonstrates revolving a path into a solid" ) <|>
      OA.flag' sweepExample (OA.long "sweep" <> OA.help "demonstrates sweeping a shape along a path" ) <|>
      OA.flag' offsetExample (OA.long "offset" <> OA.help "demonstrates offsetting the surface of a shape" ) <|>
      OA.flag' loftExample (OA.long "loft" <> OA.help "generating a boat, defined as a loft of a series of paths" ) <|>
      OA.flag' boundingBoxExample (OA.long "bound" <> OA.help "demonstrates calculating the oriented bounding box, and axis aligned bounding box, of a shape" ) <|>
      (OA.flag' gearExample (OA.long "gear" <> OA.help "generate an involute gear") <*>
       (OA.option OA.auto (OA.long "thickness" <> OA.help "gear depth") <|> pure 1.0) <*>
       (OA.option OA.auto (OA.long "module" <> OA.help "gear module") <|> pure 5.0) <*>
       (OA.option OA.auto (OA.long "nGears" <> OA.help "number of gears") <|> pure 20) <*>
       (OA.option OA.auto (OA.long "pitch" <> OA.help "pitchAngle") 
          <|> ((* (pi/180)) <$> OA.option OA.auto (OA.long "pitchDegrees" <> OA.help "pitch angle in degrees"))
          <|> pure (20*pi/180)) 
      )) <|> 
      ( let readSolidExprDescription = 
                "load solid files, and combine them with boolean operatiors according to an expression.\n" <> 
                "filenames in expressions should be wrapped in braces {}, Expressions support brackets (),\n" <>
                "and the + * and - infix operators, meaning union, intersection and difference.\n" <>
                "eg. \"({fileA.stl}*{fileB.stl})-{fileC.stl}\"" 
         in readSolidExpressionExample <$> OA.strOption (OA.long "read-solid-expression" <> OA.help readSolidExprDescription)
      ) <|>
      (OA.flag' textExample (OA.long "text" <> OA.help "render text") <*>
       (OA.strOption (OA.long "font" <> OA.help "font path")) <*>
       (OA.option OA.auto (OA.long "size" <> OA.help "font size") <|> pure 12.0) <*>
       (OA.strOption (OA.long "content" <> OA.help "text to render") <|> pure "Waterfall CAD") <*>
       (OA.option OA.auto (OA.long "depth" <> OA.help "depth to extrude the text to") <|> pure 10.0) 
      ) <|> 
      (OA.flag' (either error pure . pathExample) (OA.long "svg-path" <> OA.help "parse an SVG path string and use it to generate a prism") <*> 
       (OA.strOption (OA.long "path-string" <> OA.help "the SVG path string")))

main :: IO ()
main = join (OA.execParser opts)
    where
        opts = OA.info ((liftA2 (=<<)  outputOption  exampleOption) OA.<**> OA.helper) 
            (OA.fullDesc
             <> OA.progDesc "generate and write a 3D model"
             <> OA.header "examples for Waterfall-CAD")
