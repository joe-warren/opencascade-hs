module DiagramGoldenTests
( diagramGoldenTests
) where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Waterfall (Diagram)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Output (ppTopElement, showTopElement)
import Text.XML.Light as XML
import DarkModeSVG (darkModeSVG, writeDarkModeSVG)
import qualified Graphics.Svg as Svg
import Data.Maybe (fromMaybe)
import qualified Waterfall as W
import Linear
import qualified CsgExample
import Control.Monad.Except (runExceptT, liftEither)
import qualified Data.ByteString.Char8 as BSC8
import qualified Graphics.Rasterific.Svg as Svg
import qualified Graphics.Text.TrueType as FF
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import qualified Codec.Picture as JP
import Control.Lens
import Data.Monoid (Sum (getSum))
import Codec.Picture (imagePixels)
import qualified Waterfall.SVG.ToSVG as Waterfall.SVG
import System.FilePath (takeBaseName)
import qualified GearExample
import qualified RevolutionExample
import qualified SweepExample
import qualified OffsetExample
import qualified TextExample
import qualified BoundingBoxExample
import qualified LoftExample
import qualified TwoDBooleansExample
import qualified PlatonicSolidsExample
import qualified TakePathFractionExample

xmlToSvg :: String -> XML.Element -> Either String Svg.Document
xmlToSvg fileDesc = maybe (Left $ "failed to parse " <> fileDesc) Right . Svg.parseSvgFile "file.svg" . BSC8.pack . XML.showTopElement 

zippingTraversal :: JP.Pixel px => Traversal' (JP.Image px, JP.Image px) (px, px)
zippingTraversal =
    alongside (partsOf JP.imagePixels) (partsOf JP.imagePixels) 
    . iso (uncurry zip) unzip
    . traverse 

compareOutput :: FilePath -> XML.Element ->  XML.Element -> IO (Maybe String)
compareOutput inputPath expected actual = 
    let extract (Left s) = Just s
        extract (Right ()) = Nothing 
        render = Svg.renderSvgDocument
            FF.emptyFontCache
            Nothing -- Document Size 
            72 -- DPI, a so-so DPI for screens, higher would just mean making the test less sensitive
        size a = (JP.imageWidth a, JP.imageHeight a)
        pixelThreshold = 10 :: Integer
        comparePixels (JP.PixelRGBA8 r1 g1 b1 a1) (JP.PixelRGBA8 r2 g2 b2 a2) = 
            let c x y = abs (fromIntegral x - fromIntegral y)
            in (c r1 r2 + c g1 g2 + c b1 b2 + c a1 a2 ) < pixelThreshold
        countMismatchedPixel (x, y) = if comparePixels x y then 0 else (1 :: Sum Integer)
        colourMismatchedPixel (x, y) = if comparePixels x y then x else JP.PixelRGBA8 255 0 0 255
        dup a = (a, a)
    in fmap extract . runExceptT $ do 
        expectedSvg <- liftEither $ xmlToSvg "expected" expected
        actualSvg <- liftEither $ xmlToSvg "acutal" actual
        (expectedRendered, _) <- liftIO $ render expectedSvg
        (acutalRendered, _) <- liftIO $ render actualSvg

        unless (size expectedRendered == size acutalRendered) . liftEither . Left $ 
            ("incompatible sizes, expected: " <> show (size expectedRendered) <> ", actual " <> show (size acutalRendered))
        let (width, height) = size expectedRendered

        let mismatchedCount = getSum $ foldMapOf zippingTraversal countMismatchedPixel (expectedRendered, acutalRendered)
        unless (mismatchedCount < 10) $ do
            let makePath kind = "./test-results/" <> takeBaseName inputPath <> "." <> kind <> ".png"
            let expectedPath = makePath "expected"
            let diffPath = makePath "diff"
            let actualPath = makePath "actual"
            let totalPixels = width * height
            let diffImage = 
                    (expectedRendered, acutalRendered) 
                        & zippingTraversal %~ (dup . colourMismatchedPixel)
                        & (^. _1)
            liftIO $ JP.writePng expectedPath expectedRendered
            liftIO $ JP.writePng diffPath diffImage
            liftIO $ JP.writePng actualPath acutalRendered

            liftEither . Left $ (show mismatchedCount <> "/" <> show totalPixels<> " pixels didn't match, details written to " <> diffPath)
    
doTest :: TestName -> FilePath -> IO Diagram -> TestTree
doTest testName goldenPath makeDiagram = 
    let readGoldenFile = fromMaybe (error $ "failed to read " <> goldenPath) . parseXMLDoc <$> readFile goldenPath
        updateGoldenFile = writeFile goldenPath . ppTopElement
    in goldenTest 
        testName
        readGoldenFile
        (darkModeSVG <$> makeDiagram)
        (compareOutput goldenPath)
        updateGoldenFile
    
standardSolidDiagram :: W.Solid -> W.Diagram
standardSolidDiagram = withWidth 800 . W.solidDiagram (V3 2 3 1)


normalizeSize :: (V2 Double -> Double) -> Double -> W.Diagram -> W.Diagram
normalizeSize getter target d = 
    case W.diagramBoundingBox d of 
        Just (lo, hi) -> 
            let s = getter (hi - lo)
            in W.uScale2D (target / s) d
        Nothing -> d

withHeight :: Double -> W.Diagram -> W.Diagram
withHeight = normalizeSize (^. _y) 

withWidth :: Double -> W.Diagram -> W.Diagram
withWidth = normalizeSize (^. _x) 

diagramGoldenTests :: TestTree
diagramGoldenTests = testGroup "Diagram Golden Tests" 
    [ doTest "CSG" "../images/csg.svg" (pure . withHeight 200 . standardSolidDiagram $ CsgExample.csgExample)
    , doTest "Gear" "../images/gear.svg" (pure . standardSolidDiagram $ GearExample.gearExample 1 5 20 (20*pi/180))
    , doTest "Revolution" "../images/revolution.svg" (pure . withHeight 200 . standardSolidDiagram $ RevolutionExample.revolutionExample)
    , doTest "Sweep" "../images/sweep.svg" (pure . withHeight 200 . standardSolidDiagram $ SweepExample.sweepExample)
    , doTest "Offset" "../images/offset.svg" (pure . standardSolidDiagram $ OffsetExample.offsetExample)
    , doTest "Text" "../images/text.svg" 
        (standardSolidDiagram <$> TextExample.textExample 
            "../images/fonts/varela/VarelaRound-Regular.ttf" 
            12.0
            "Waterfall-CAD"
            10
        )
    , doTest "Bound" "../images/bounding-boxes.svg" (pure . withHeight 200 . standardSolidDiagram $ BoundingBoxExample.boundingBoxExample)
    , doTest "Loft" "../images/loft.svg" (pure . standardSolidDiagram $ LoftExample.loftExample)
    , doTest "2D Booleans" "../images/2d-booleans.svg" (pure . withHeight 200 . standardSolidDiagram $ TwoDBooleansExample.twoDBooleansExample)
    , doTest "Platonic Solids" "../images/platonic.svg" (pure . standardSolidDiagram $ PlatonicSolidsExample.platonicSolidsExample)
    , doTest "Take Path Fraction" "../images/takePathFraction.svg" (pure . standardSolidDiagram $ TakePathFractionExample.takePathFractionExample)

    ]