{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeRevol 
( MakeRevol
, fromShapeAndAx1
) where

import OpenCascade.BRepPrimAPI.Types (MakeRevol)
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeRevol)

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import OpenCascade.Internal.Bool (boolToCBool)
import Foreign.C
import Foreign.Ptr
import Data.Acquire

foreign import capi unsafe "hs_BRepPrimAPI_MakeRevol.h hs_new_BRepPrimAPI_MakeRevol_fromShapeAndAx1" rawFromShapeAndAx1 :: Ptr TopoDS.Shape -> Ptr GP.Ax1 -> CBool -> IO (Ptr MakeRevol)

fromShapeAndAx1 :: Ptr TopoDS.Shape -> Ptr GP.Ax1 -> Bool -> Acquire (Ptr MakeRevol)
fromShapeAndAx1 shape axis copy = mkAcquire (rawFromShapeAndAx1 shape axis (boolToCBool copy)) deleteMakeRevol
