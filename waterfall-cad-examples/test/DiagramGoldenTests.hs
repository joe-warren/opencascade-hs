module DiagramGoldenTests
( diagramGoldenTests
) where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.XML.Light as XML
import DarkModeSVG (darkModeSVG)
import qualified Graphics.Svg as Svg
import Data.Maybe (fromMaybe)
import Waterfall.Diagram (Diagram)
import qualified Waterfall.Diagram as Diagram
import qualified Waterfall.TwoD.Transforms as TwoD.Transforms
import Waterfall.Solids (Solid)
import Linear
import qualified CsgExample
import Control.Monad.Except (runExceptT, liftEither)
import qualified Data.ByteString.Char8 as BSC8
import qualified Graphics.Rasterific.Svg as RSvg
import qualified Graphics.Text.TrueType as FF
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, join)
import qualified Codec.Picture as JP
import Control.Lens
import Data.Monoid (Sum (getSum))
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

pixelDiffThreshold :: Integer
pixelDiffThreshold = 10

pixelCountThreshold :: Integer
pixelCountThreshold = 50

comparePixels :: JP.PixelRGBA8 -> JP.PixelRGBA8 -> Bool
comparePixels (JP.PixelRGBA8 r1 g1 b1 a1) (JP.PixelRGBA8 r2 g2 b2 a2) = 
    let c x y = abs (fromIntegral x - fromIntegral y)
    in (c r1 r2 + c g1 g2 + c b1 b2 + c a1 a2 ) < pixelDiffThreshold

size :: JP.Image px -> (Int, Int)
size a = (JP.imageWidth a, JP.imageHeight a)

render :: Svg.Document -> IO (JP.Image JP.PixelRGBA8, RSvg.LoadedElements)
render = RSvg.renderSvgDocument
    FF.emptyFontCache
    Nothing -- Document Size 
    72 -- DPI, a so-so DPI for screens, higher would just mean making the test less sensitive

countMismatchedPixel :: (JP.PixelRGBA8, JP.PixelRGBA8) -> Sum Integer
countMismatchedPixel (x, y) = if comparePixels x y then 0 else 1

colourMismatchedPixel :: (JP.PixelRGBA8, JP.PixelRGBA8) -> JP.PixelRGBA8
colourMismatchedPixel (x, y) = if comparePixels x y then x else JP.PixelRGBA8 255 0 0 255

dup :: a -> (a, a)        
dup = join (,)

compareOutput :: FilePath -> XML.Element ->  XML.Element -> IO (Maybe String)
compareOutput inputPath expected actual = 
    let extract (Left s) = Just s
        extract (Right ()) = Nothing 
    in fmap extract . runExceptT $ do 
        expectedSvg <- liftEither $ xmlToSvg "expected" expected
        actualSvg <- liftEither $ xmlToSvg "actual" actual
        (expectedRendered, _) <- liftIO $ render expectedSvg
        (actualRendered, _) <- liftIO $ render actualSvg
        
        let makePath kind = "./test-results/" <> takeBaseName inputPath <> "." <> kind <> ".png"
        let expectedPath = makePath "expected"
        let diffPath = makePath "diff"
        let actualPath = makePath "actual"

        unless (size expectedRendered == size actualRendered) $ do 
            liftIO $ JP.writePng expectedPath expectedRendered
            liftIO $ JP.writePng actualPath actualRendered
            liftEither . Left $ 
                ( "incompatible sizes, expected: "
                <> show (size expectedRendered) 
                <> ", actual " 
                <> show (size actualRendered)
                <> ", actual written to "
                <> actualPath)

        let (width, height) = size expectedRendered

        let mismatchedCount = getSum $ foldMapOf zippingTraversal countMismatchedPixel (expectedRendered, actualRendered)
        unless (mismatchedCount < pixelCountThreshold) $ do
            let totalPixels = width * height
            let diffImage = 
                    (expectedRendered, actualRendered) 
                        & zippingTraversal %~ (dup . colourMismatchedPixel)
                        & (^. _1)
            liftIO $ JP.writePng expectedPath expectedRendered
            liftIO $ JP.writePng diffPath diffImage
            liftIO $ JP.writePng actualPath actualRendered

            liftEither . Left $
                ( show mismatchedCount <> "/" <> show totalPixels 
                <> " pixels didn't match, diff written to " 
                <> diffPath
                )
    
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
    
standardSolidDiagram :: Solid -> Diagram
standardSolidDiagram = withWidth 800 . Diagram.solidDiagram (V3 2 3 1)

normalizeSize :: (V2 Double -> Double) -> Double -> Diagram -> Diagram
normalizeSize getter target d = 
    case Diagram.diagramBoundingBox d of 
        Just (lo, hi) -> 
            let s = getter (hi - lo)
            in TwoD.Transforms.uScale2D (target / s) d
        Nothing -> d

withHeight :: Double -> Diagram -> Diagram
withHeight = normalizeSize (^. _y) 

withWidth :: Double -> Diagram -> Diagram
withWidth = normalizeSize (^. _x) 

solidTest :: TestName -> FilePath -> Solid -> TestTree
solidTest name filepath solid =
    doTest name filepath (pure . standardSolidDiagram $ solid)

smallSolidTest :: TestName -> FilePath -> Solid -> TestTree
smallSolidTest name filepath solid =
    doTest name filepath (pure . withHeight 200 . standardSolidDiagram $ solid)

diagramGoldenTests :: TestTree
diagramGoldenTests = testGroup "Diagram Golden Tests" 
    [ smallSolidTest "CSG" "../images/csg.svg" CsgExample.csgExample
    , solidTest "Gear" "../images/gear.svg" $ GearExample.gearExample 1 5 20 (20*pi/180)
    , smallSolidTest "Revolution" "../images/revolution.svg" RevolutionExample.revolutionExample
    , smallSolidTest "Sweep" "../images/sweep.svg" SweepExample.sweepExample
    , solidTest "Offset" "../images/offset.svg" OffsetExample.offsetExample
    , doTest "Text" "../images/text.svg" 
        (standardSolidDiagram <$> TextExample.textExample 
            "../images/fonts/varela/VarelaRound-Regular.ttf" 
            12.0
            "Waterfall-CAD"
            10
        )
    , smallSolidTest "Bound" "../images/bounding-boxes.svg" BoundingBoxExample.boundingBoxExample
    , solidTest "Loft" "../images/loft.svg" LoftExample.loftExample
    , smallSolidTest "2D Booleans" "../images/2d-booleans.svg"  TwoDBooleansExample.twoDBooleansExample
    , solidTest "Platonic Solids" "../images/platonic.svg" PlatonicSolidsExample.platonicSolidsExample
    , solidTest "Take Path Fraction" "../images/takePathFraction.svg" TakePathFractionExample.takePathFractionExample
    ]