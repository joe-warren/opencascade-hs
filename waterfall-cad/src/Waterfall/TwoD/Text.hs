module Waterfall.TwoD.Text
( text
, Font 
, FontAspect (..)
, fontFromPath
, fontFromSystem
) where

import qualified Waterfall.TwoD.Internal.Shape as Shape
import Waterfall.Internal.Finalizers (acquireForeignPtr, fromAcquirePtr, unsafeFromAcquirePtr)
import Waterfall.IO (WaterfallIOException (..), WaterfallIOExceptionCause (FileError))
import qualified OpenCascade.GP.Ax3 as GP.Ax3
import qualified OpenCascade.Font.BRepFont as BRepFont
import qualified OpenCascade.Font.BRepTextBuilder as BRepTextBuilder
import qualified OpenCascade.Graphic3D.VerticalTextAlignment as VTA
import qualified OpenCascade.Graphic3D.HorizontalTextAlignment as HTA
import Foreign.ForeignPtr (ForeignPtr)
import Control.Monad (unless)
import OpenCascade.Font.FontAspect (FontAspect (..))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)

newtype Font = Font { rawFont :: ForeignPtr BRepFont.BRepFont }

-- | Create a font from a filepath and a font size 
fontFromPath :: FilePath -> Double -> IO Font 
fontFromPath fontpath size = fmap Font . fromAcquirePtr $ do
    bRepFont <- BRepFont.new
    fontOk <- liftIO $ BRepFont.initFromPathAndSize bRepFont fontpath size
    unless fontOk $ liftIO (throwIO (WaterfallIOException FileError fontpath))
    return bRepFont

-- | Create a font from a system font name, aspect, and size
fontFromSystem :: String -> FontAspect -> Double -> IO Font 
fontFromSystem name aspect size = fmap Font . fromAcquirePtr $ do
    bRepFont <- BRepFont.new
    fontOk <- liftIO $ BRepFont.initFromNameAspectAndSize bRepFont name aspect size
    unless fontOk . liftIO $ throwIO $ WaterfallIOException FileError (name <> "::" <> show aspect)
    return bRepFont

-- | Render text, using a given font
text :: Font -> String -> Shape.Shape 
text font content = Shape.Shape . unsafeFromAcquirePtr $ do
    axis <- GP.Ax3.new
    bRepFont <- acquireForeignPtr . rawFont $ font
    builder <- BRepTextBuilder.new
    BRepTextBuilder.perform builder bRepFont content axis HTA.Center VTA.Center

