{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeFix.Solid
( Solid
, fromSolid
, perform
, solid
, status
) where

import OpenCascade.ShapeFix.Types (Solid)
import OpenCascade.ShapeFix.Internal.Destructors (deleteSolid)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.Message.Types as Message
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..))
import OpenCascade.Internal.Bool (cBoolToBool)
import OpenCascade.ShapeExtend.Status (Status)

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_new_ShapeFix_Solid_fromSolid" rawFromSolid :: Ptr TopoDS.Solid -> IO (Ptr Solid)

fromSolid :: Ptr TopoDS.Solid -> Acquire (Ptr Solid)
fromSolid s = mkAcquire (rawFromSolid s) deleteSolid


foreign import capi unsafe "hs_ShapeFix_Solid.h hs_ShapeFix_Solid_perform" rawPerform :: Ptr Solid -> Ptr Message.ProgressRange -> IO (CBool)

perform :: Ptr Solid -> Ptr Message.ProgressRange-> IO Bool
perform = (fmap cBoolToBool .) . rawPerform

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_ShapeFix_Solid_solid" rawSolid :: Ptr Solid -> IO (Ptr TopoDS.Shape)

solid :: Ptr Solid -> Acquire (Ptr TopoDS.Shape)
solid s = mkAcquire (rawSolid s) deleteShape

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_ShapeFix_Solid_status" rawStatus :: Ptr Solid -> CInt -> IO CBool

status :: Ptr Solid  -> Status -> IO Bool
status s toCheck = cBoolToBool <$> rawStatus s (fromIntegral . fromEnum $ toCheck) 
