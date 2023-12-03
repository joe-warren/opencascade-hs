module Waterfall.TwoD.Text
( text 
) where

import qualified Waterfall.TwoD.Internal.Shape as Shape
import qualified OpenCascade.GP.Ax3 as GP.Ax3
import qualified OpenCascade.Font.BRepFont as BRepFont
import qualified OpenCascade.Font.BRepTextBuilder as BRepTextBuilder
import qualified OpenCascade.Graphic3D.VerticalTextAlignment as VTA
import qualified OpenCascade.Graphic3D.HorizontalTextAlignment as HTA

-- | Render text, using the font from the provided filepath, at a given size.
--
-- The IO of actually loading the font/checking the file exists is defered 
-- until the Shape is actually used
text :: FilePath -> Double -> String -> Shape.Shape 
text fontpath size content = Shape.Shape $ do
    axis <- GP.Ax3.new
    font <- BRepFont.fromPathAndSize fontpath size
    builder <- BRepTextBuilder.new
    BRepTextBuilder.perform builder font content axis HTA.Center VTA.Center

