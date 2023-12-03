{-# LANGUAGE  CApiFFI #-}
module OpenCascade.Font.BRepTextBuilder
( BRepTextBuilder
, new
, perform
) where

import OpenCascade.Font.Types
import OpenCascade.Font.Internal.Destructors
import Foreign.Ptr
import Foreign.C
import Data.Acquire
import qualified OpenCascade.Graphic3D.VerticalTextAlignment as VTA
import qualified OpenCascade.Graphic3D.HorizontalTextAlignment as HTA
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import Foreign.C (withCString)

foreign import capi unsafe "hs_Font_BRepTextBuilder.h hs_new_Font_BRepTextBuilder" rawNew :: IO (Ptr BRepTextBuilder)

new :: Acquire (Ptr BRepTextBuilder)
new = mkAcquire rawNew deleteBRepTextBuilder


foreign import capi unsafe "hs_Font_BRepTextBuilder.h hs_Font_BRepTextBuilder_perform" rawPerform :: Ptr BRepTextBuilder -> Ptr BRepFont -> CString -> Ptr GP.Ax3 -> CInt -> CInt -> IO (Ptr TopoDS.Shape)

perform :: Ptr BRepTextBuilder -> Ptr BRepFont -> String -> Ptr GP.Ax3 -> HTA.HorizontalTextAlignment -> VTA.VerticalTextAlignment -> Acquire (Ptr TopoDS.Shape)
perform builder font str axis hAlign vAlign =
     mkAcquire 
        (withCString str $ \s -> rawPerform builder font s axis (fromIntegral . fromEnum $ hAlign) (fromIntegral . fromEnum $ vAlign)) 
        deleteShape
