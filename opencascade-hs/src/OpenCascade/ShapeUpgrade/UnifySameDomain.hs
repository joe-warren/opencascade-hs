{-# LANGUAGE CApiFFI #-}
module OpenCascade.ShapeUpgrade.UnifySameDomain
( UnifySameDomain
, fromShape
, setSafeInputMode
, allowInternalEdges
, build
, shape
) where

import OpenCascade.ShapeUpgrade.Types (UnifySameDomain)
import OpenCascade.ShapeUpgrade.Internal.Destructors (deleteUnifySameDomain)
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)
import Data.Acquire (Acquire, mkAcquire)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..))

foreign import capi unsafe "hs_ShapeUpgrade_UnifySameDomain.h hs_new_ShapeUpgrade_UnifySameDomain_fromShape" rawFromShape
    :: Ptr TopoDS.Shape
    -> CBool
    -> CBool
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr UnifySameDomain)

fromShape :: Ptr TopoDS.Shape -> Bool -> Bool -> Bool -> Acquire (Ptr UnifySameDomain)
fromShape s unifyEdges unifyFaces concatBSplines =
    mkAcquire
        (wrapException $ rawFromShape s (boolToCBool unifyEdges) (boolToCBool unifyFaces) (boolToCBool concatBSplines))
        deleteUnifySameDomain

foreign import capi unsafe "hs_ShapeUpgrade_UnifySameDomain.h hs_ShapeUpgrade_UnifySameDomain_setSafeInputMode" rawSetSafeInputMode :: Ptr UnifySameDomain -> CBool -> IO ()

setSafeInputMode :: Ptr UnifySameDomain -> Bool -> IO ()
setSafeInputMode unify = rawSetSafeInputMode unify . boolToCBool

foreign import capi unsafe "hs_ShapeUpgrade_UnifySameDomain.h hs_ShapeUpgrade_UnifySameDomain_allowInternalEdges" rawAllowInternalEdges :: Ptr UnifySameDomain -> CBool -> IO ()

allowInternalEdges :: Ptr UnifySameDomain -> Bool -> IO ()
allowInternalEdges unify = rawAllowInternalEdges unify . boolToCBool

foreign import capi unsafe "hs_ShapeUpgrade_UnifySameDomain.h hs_ShapeUpgrade_UnifySameDomain_build" rawBuild :: Ptr UnifySameDomain -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

build :: Ptr UnifySameDomain -> IO ()
build unify = wrapException $ rawBuild unify

foreign import capi unsafe "hs_ShapeUpgrade_UnifySameDomain.h hs_ShapeUpgrade_UnifySameDomain_shape" rawShape :: Ptr UnifySameDomain -> IO (Ptr TopoDS.Shape)

shape :: Ptr UnifySameDomain -> Acquire (Ptr TopoDS.Shape)
shape unify = mkAcquire (rawShape unify) deleteShape
