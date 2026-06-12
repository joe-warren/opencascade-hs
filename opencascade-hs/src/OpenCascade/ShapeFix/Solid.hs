{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeFix.Solid
( Solid
, new
, fromSolid
, solidFromShell
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
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Exception (wrapException)


foreign import capi unsafe "hs_ShapeFix_Solid.h hs_new_ShapeFix_Solid" rawNew :: IO (Ptr Solid)

new :: Acquire (Ptr Solid)
new = mkAcquire rawNew deleteSolid

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_new_ShapeFix_Solid_fromSolid" rawFromSolid
    :: Ptr TopoDS.Solid
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr Solid)

fromSolid :: Ptr TopoDS.Solid -> Acquire (Ptr Solid)
fromSolid s = mkAcquire (wrapException $ rawFromSolid s) deleteSolid

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_ShapeFix_Solid_perform" rawPerform
    :: Ptr Solid
    -> Ptr Message.ProgressRange
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (CBool)

perform :: Ptr Solid -> Ptr Message.ProgressRange-> IO Bool
perform s progress = cBoolToBool <$> wrapException (rawPerform s progress)

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_ShapeFix_Solid_solid" rawSolid
    :: Ptr Solid
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

solid :: Ptr Solid -> Acquire (Ptr TopoDS.Shape)
solid s = mkAcquire (wrapException $ rawSolid s) deleteShape

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_ShapeFix_Solid_solidFromShell" rawSolidFromShell
    :: Ptr Solid
    -> Ptr TopoDS.Shell
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Solid)

solidFromShell :: Ptr Solid -> Ptr TopoDS.Shell -> Acquire (Ptr TopoDS.Solid)
solidFromShell s shell = mkAcquire (wrapException $ rawSolidFromShell s shell) (deleteShape . upcast)

foreign import capi unsafe "hs_ShapeFix_Solid.h hs_ShapeFix_Solid_status" rawStatus :: Ptr Solid -> CInt -> IO CBool

status :: Ptr Solid  -> Status -> IO Bool
status s toCheck = cBoolToBool <$> rawStatus s (fromIntegral . fromEnum $ toCheck) 
