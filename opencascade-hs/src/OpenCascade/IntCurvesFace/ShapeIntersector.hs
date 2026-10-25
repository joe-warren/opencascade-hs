{-# LANGUAGE CApiFFI #-}
module OpenCascade.IntCurvesFace.ShapeIntersector
( ShapeIntersector
, new
, load
, perform
, performNearest
, isDone
, nbPnt
, uParameter
, vParameter
, wParameter
, pnt
, face
) where

import OpenCascade.IntCurvesFace.Types (ShapeIntersector)
import OpenCascade.IntCurvesFace.Internal.Destructors (deleteShapeIntersector)
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import qualified OpenCascade.GP.Types as GP
import OpenCascade.GP.Internal.Destructors (deletePnt)
import OpenCascade.Inheritance (upcast)
import OpenCascade.Internal.Exception (wrapException)
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.Ptr (Ptr)
import Foreign.C (CInt (..), CBool (..), CDouble (..))
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_new_IntCurvesFace_ShapeIntersector" rawNew :: IO (Ptr ShapeIntersector)

new :: Acquire (Ptr ShapeIntersector)
new = mkAcquire rawNew deleteShapeIntersector

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_Load" rawLoad :: Ptr ShapeIntersector -> Ptr TopoDS.Shape -> CDouble -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

load :: Ptr ShapeIntersector -> Ptr TopoDS.Shape -> Double -> IO ()
load intersector shape tol = wrapException $ rawLoad intersector shape (coerce tol)

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_Perform" rawPerform :: Ptr ShapeIntersector -> Ptr GP.Lin -> CDouble -> CDouble -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

perform :: Ptr ShapeIntersector -> Ptr GP.Lin -> Double -> Double -> IO ()
perform intersector line pInf pSup = wrapException $ rawPerform intersector line (coerce pInf) (coerce pSup)

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_PerformNearest" rawPerformNearest :: Ptr ShapeIntersector -> Ptr GP.Lin -> CDouble -> CDouble -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

performNearest :: Ptr ShapeIntersector -> Ptr GP.Lin -> Double -> Double -> IO ()
performNearest intersector line pInf pSup = wrapException $ rawPerformNearest intersector line (coerce pInf) (coerce pSup)

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_IsDone" rawIsDone :: Ptr ShapeIntersector -> IO CBool

isDone :: Ptr ShapeIntersector -> IO Bool
isDone intersector = cBoolToBool <$> rawIsDone intersector

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_NbPnt" rawNbPnt :: Ptr ShapeIntersector -> IO CInt

nbPnt :: Ptr ShapeIntersector -> IO Int
nbPnt intersector = fromIntegral <$> rawNbPnt intersector

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_UParameter" rawUParameter :: Ptr ShapeIntersector -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO CDouble

uParameter :: Ptr ShapeIntersector -> Int -> IO Double
uParameter intersector index = coerce <$> wrapException (rawUParameter intersector (fromIntegral index))

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_VParameter" rawVParameter :: Ptr ShapeIntersector -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO CDouble

vParameter :: Ptr ShapeIntersector -> Int -> IO Double
vParameter intersector index = coerce <$> wrapException (rawVParameter intersector (fromIntegral index))

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_WParameter" rawWParameter :: Ptr ShapeIntersector -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO CDouble

wParameter :: Ptr ShapeIntersector -> Int -> IO Double
wParameter intersector index = coerce <$> wrapException (rawWParameter intersector (fromIntegral index))

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_Pnt" rawPnt :: Ptr ShapeIntersector -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr GP.Pnt)

pnt :: Ptr ShapeIntersector -> Int -> Acquire (Ptr GP.Pnt)
pnt intersector index = mkAcquire (wrapException $ rawPnt intersector (fromIntegral index)) deletePnt

foreign import capi unsafe "hs_IntCurvesFace_ShapeIntersector.h hs_IntCurvesFace_ShapeIntersector_Face" rawFace :: Ptr ShapeIntersector -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr TopoDS.Face)

face :: Ptr ShapeIntersector -> Int -> Acquire (Ptr TopoDS.Face)
face intersector index = mkAcquire (wrapException $ rawFace intersector (fromIntegral index)) (deleteShape . upcast)
