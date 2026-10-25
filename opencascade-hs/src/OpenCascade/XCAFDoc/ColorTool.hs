{-# LANGUAGE CApiFFI #-}
module OpenCascade.XCAFDoc.ColorTool
( ColorTool
, setColor
, setShapeColor
) where

import OpenCascade.XCAFDoc.Types (ColorTool)
import OpenCascade.XCAFDoc.ColorType (ColorType)
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.Quantity.Types as Quantity
import OpenCascade.Handle (Handle)
import OpenCascade.TDF.Types (Label)
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

foreign import capi unsafe "hs_XCAFDoc_ColorTool.h hs_XCAFDoc_ColorTool_setColor" rawSetColor
    :: Ptr (Handle ColorTool)
    -> Ptr Label
    -> Ptr Quantity.Color
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

setColor :: Ptr (Handle ColorTool) -> Ptr Label -> Ptr Quantity.Color -> ColorType -> IO ()
setColor tool label color colorType = wrapException $ rawSetColor tool label color (fromIntegral . fromEnum $ colorType)

foreign import capi unsafe "hs_XCAFDoc_ColorTool.h hs_XCAFDoc_ColorTool_setShapeColor" rawSetShapeColor
    :: Ptr (Handle ColorTool)
    -> Ptr TopoDS.Shape
    -> Ptr Quantity.Color
    -> CInt
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO CBool

setShapeColor :: Ptr (Handle ColorTool) -> Ptr TopoDS.Shape -> Ptr Quantity.Color -> ColorType -> IO Bool
setShapeColor tool shape color colorType = cBoolToBool <$> wrapException (rawSetShapeColor tool shape color (fromIntegral . fromEnum $ colorType))
