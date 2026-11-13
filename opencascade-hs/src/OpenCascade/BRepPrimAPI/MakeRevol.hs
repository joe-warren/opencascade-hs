{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakeRevol 
( MakeRevol
, fromShapeAndAx1
, fromShapeAx1AndAngle
) where

import OpenCascade.BRepPrimAPI.Types (MakeRevol)
import OpenCascade.BRepPrimAPI.Internal.Destructors (deleteMakeRevol)

import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.GP as GP
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BRepPrimAPI_MakeRevol.h hs_new_BRepPrimAPI_MakeRevol_fromShapeAndAx1" rawFromShapeAndAx1
    :: Ptr TopoDS.Shape
    -> Ptr GP.Ax1
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr MakeRevol)

fromShapeAndAx1 :: Ptr TopoDS.Shape -> Ptr GP.Ax1 -> Bool -> Acquire (Ptr MakeRevol)
fromShapeAndAx1 shape axis copy = mkAcquire (wrapException $ rawFromShapeAndAx1 shape axis (boolToCBool copy)) deleteMakeRevol

foreign import capi unsafe "hs_BRepPrimAPI_MakeRevol.h hs_new_BRepPrimAPI_MakeRevol_fromShapeAx1AndAngle" rawFromShapeAndAx1AndAngle
    :: Ptr TopoDS.Shape
    -> Ptr GP.Ax1
    -> CDouble
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr MakeRevol)

fromShapeAx1AndAngle :: Ptr TopoDS.Shape -> Ptr GP.Ax1 -> Double -> Bool -> Acquire (Ptr MakeRevol)
fromShapeAx1AndAngle shape axis angle copy = 
    mkAcquire 
        (wrapException $ 
            rawFromShapeAndAx1AndAngle 
                shape 
                axis 
                (coerce angle) 
                (boolToCBool copy)
        )
        deleteMakeRevol

