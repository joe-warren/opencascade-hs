{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeFace 
( MakeFace
, new 
, fromFace
, fromSurface
, fromSurfaceAndBounds
, fromSurfaceAndWire
, add
, isDone
, error
) where

import Prelude hiding (error)
import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Destructors
import OpenCascade.Handle
import OpenCascade.Inheritance
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified OpenCascade.Geom as Geom
import OpenCascade.BRepBuilderAPI.FaceError (FaceError)
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_new_BRepBuilderAPI_MakeFace" rawNew :: IO (Ptr MakeFace)

new :: Acquire (Ptr MakeFace)
new = mkAcquire rawNew deleteMakeFace

-- fromFace

foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_new_BRepBuilderAPI_MakeFace_fromFace" rawFromFace :: Ptr TopoDS.Face ->  IO (Ptr MakeFace)

fromFace :: Ptr TopoDS.Face -> Acquire (Ptr MakeFace)
fromFace face = mkAcquire (rawFromFace face) deleteMakeFace

-- fromSurface

foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_new_BRepBuilderAPI_MakeFace_fromSurface" rawFromSurface :: Ptr (Handle (Geom.Surface)) -> CDouble -> IO (Ptr MakeFace)

fromSurface :: Ptr (Handle Geom.Surface) -> Double -> Acquire (Ptr MakeFace)
fromSurface surf tolerance = mkAcquire (rawFromSurface surf (CDouble tolerance)) deleteMakeFace

-- fromSurfaceAndBounds

foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_new_BRepBuilderAPI_MakeFace_fromSurfaceAndBounds" rawFromSurfaceAndBounds :: Ptr (Handle (Geom.Surface)) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr MakeFace)

fromSurfaceAndBounds :: Ptr (Handle Geom.Surface) -> Double -> Double -> Double -> Double -> Double -> Acquire (Ptr MakeFace)
fromSurfaceAndBounds surf uMin uMax vMin vMax tolerance = mkAcquire (rawFromSurfaceAndBounds surf (CDouble uMin) (CDouble uMax) (CDouble vMin) (CDouble vMax) (CDouble tolerance)) deleteMakeFace

-- fromSurfaceAndWire
--
foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_new_BRepBuilderAPI_MakeFace_fromSurfaceAndWire" rawFromSurfaceAndWire :: Ptr (Handle (Geom.Surface)) -> Ptr TopoDS.Wire -> CBool -> IO (Ptr MakeFace)

fromSurfaceAndWire :: Ptr (Handle Geom.Surface) -> Ptr TopoDS.Wire -> Bool -> Acquire (Ptr MakeFace)
fromSurfaceAndWire surf wire inside = mkAcquire (rawFromSurfaceAndWire surf wire (boolToCBool inside)) deleteMakeFace

-- add 
foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_BRepBuilderAPI_MakeFace_Add" add :: Ptr MakeFace -> Ptr TopoDS.Wire -> IO ()

-- isDone
--
foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_BRepBuilderAPI_MakeFace_IsDone" rawIsDone :: Ptr MakeFace -> IO (CBool)

isDone :: Ptr MakeFace -> IO Bool
isDone p = cBoolToBool <$> rawIsDone p

-- error
--
foreign import capi unsafe "hs_BRepBuilderAPI_MakeFace.h hs_BRepBuilderAPI_MakeFace_Error" rawError :: Ptr MakeFace -> IO (CInt)

error :: Ptr MakeFace -> IO FaceError 
error p = toEnum . fromIntegral <$> rawError p
