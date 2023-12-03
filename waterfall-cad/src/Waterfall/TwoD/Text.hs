module Waterfall.TwoD.Text
( text
, Font 
, FontAspect (..)
, fontFromPath
, fontFromSystem
) where

import qualified Waterfall.TwoD.Internal.Shape as Shape
import qualified OpenCascade.GP.Ax3 as GP.Ax3
import qualified OpenCascade.Font.BRepFont as BRepFont
import qualified OpenCascade.Font.BRepTextBuilder as BRepTextBuilder
import qualified OpenCascade.Graphic3D.VerticalTextAlignment as VTA
import qualified OpenCascade.Graphic3D.HorizontalTextAlignment as HTA
import Foreign.Ptr 
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import OpenCascade.Font.FontAspect (FontAspect (..))

newtype Font = Font {initFont :: Ptr BRepFont.BRepFont -> IO ()}

-- | create a font from a filepath and a font size 
fontFromPath :: FilePath -> Double -> Font 
fontFromPath fontpath size = Font $ \font -> do
    fontOk <- BRepFont.initFromPathAndSize font fontpath size
    unless (fontOk) $ error ("Unable to initialize font from filepath: " <> fontpath)

-- | Create a font from a system font name, aspect, and size
fontFromSystem :: String -> FontAspect -> Double -> Font 
fontFromSystem name aspect size = Font $ \font -> do
    fontOk <- BRepFont.initFromNameAspectAndSize font name aspect size
    unless (fontOk) $ error ("Unable to initialize system font with name: " <> name <> ", and aspect" <> show aspect)

-- | Render text, using the font from the provided filepath, at a given size.
--
-- The IO of actually loading the font/checking the file exists is defered 
-- until the Shape is actually used
text :: Font -> String -> Shape.Shape 
text font content = Shape.Shape $ do
    axis <- GP.Ax3.new
    brepfont <- BRepFont.new
    liftIO $ initFont font brepfont
    builder <- BRepTextBuilder.new
    BRepTextBuilder.perform builder brepfont content axis HTA.Center VTA.Center

