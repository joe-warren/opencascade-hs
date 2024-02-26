module Waterfall.TwoD.Text
( text
, Font 
, FontAspect (..)
, fontFromPath
, fontFromSystem
) where

import qualified Waterfall.TwoD.Internal.Shape as Shape
import Waterfall.Internal.Finalizers (toAcquire, fromAcquire, unsafeFromAcquire)
import qualified OpenCascade.GP.Ax3 as GP.Ax3
import qualified OpenCascade.Font.BRepFont as BRepFont
import qualified OpenCascade.Font.BRepTextBuilder as BRepTextBuilder
import qualified OpenCascade.Graphic3D.VerticalTextAlignment as VTA
import qualified OpenCascade.Graphic3D.HorizontalTextAlignment as HTA
import Foreign.Ptr 
import Control.Monad (unless)
import OpenCascade.Font.FontAspect (FontAspect (..))

newtype Font = Font { rawFont :: Ptr BRepFont.BRepFont }

-- | create a font from a filepath and a font size 
fontFromPath :: FilePath -> Double -> IO Font 
fontFromPath fontpath size = do
    bRepFont <- fromAcquire $ BRepFont.new
    fontOk <- BRepFont.initFromPathAndSize bRepFont fontpath size
    unless (fontOk) $ error ("Unable to initialize font from filepath: " <> fontpath)
    return $ Font bRepFont

-- | Create a font from a system font name, aspect, and size
fontFromSystem :: String -> FontAspect -> Double -> IO Font 
fontFromSystem name aspect size = do
    bRepFont <- fromAcquire $ BRepFont.new
    fontOk <- BRepFont.initFromNameAspectAndSize bRepFont name aspect size
    unless (fontOk) $ error ("Unable to initialize system font with name: " <> name <> ", and aspect" <> show aspect)
    return $ Font bRepFont

-- | Render text, using the font from the provided filepath, at a given size.
--
-- The IO of actually loading the font/checking the file exists is defered 
-- until the Shape is actually used
text :: Font -> String -> Shape.Shape 
text font content = Shape.Shape . unsafeFromAcquire $ do
    axis <- GP.Ax3.new
    bRepFont <- toAcquire . rawFont $ font
    builder <- BRepTextBuilder.new
    BRepTextBuilder.perform builder bRepFont content axis HTA.Center VTA.Center

