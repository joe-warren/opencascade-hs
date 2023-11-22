{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepPrimAPI.MakePrism
( fromVec
, fromDir
) where

import qualified OpenCascade.GP as GP
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

foreign import capi unsafe "hs_BRepPrimAPI_MakePrism.h hs_BRepPrimAPI_MakePrism_fromVec" rawFromVec :: Ptr TopoDS.Shape -> Ptr GP.Vec -> CBool -> CBool -> IO (Ptr TopoDS.Shape)

fromVec :: Ptr TopoDS.Shape -> Ptr GP.Vec -> Bool -> Bool -> Acquire (Ptr TopoDS.Shape)
fromVec shape vec copy canonize = mkAcquire (rawFromVec shape vec (boolToCBool copy) (boolToCBool canonize)) TopoDS.Destructors.deleteShape


foreign import capi unsafe "hs_BRepPrimAPI_MakePrism.h hs_BRepPrimAPI_MakePrism_fromDir" rawFromDir :: Ptr TopoDS.Shape -> Ptr GP.Dir -> CBool -> CBool -> CBool -> IO (Ptr TopoDS.Shape)

fromDir :: Ptr TopoDS.Shape -> Ptr GP.Dir -> Bool -> Bool -> Bool -> Acquire (Ptr TopoDS.Shape)
fromDir shape dir inf copy canonize = mkAcquire (rawFromDir shape dir (boolToCBool inf) (boolToCBool copy) (boolToCBool canonize)) TopoDS.Destructors.deleteShape
