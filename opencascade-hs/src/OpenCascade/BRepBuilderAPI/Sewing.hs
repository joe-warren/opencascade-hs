{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.Sewing
( Sewing
, new
, load
, add
, perform
, sewedShape
, nbFreeEdges
, nbContigousEdges
, nbMultipleEdges
) where 

import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.BRepBuilderAPI.Internal.Destructors (deleteSewing)
import OpenCascade.BRepBuilderAPI.Types (Sewing)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..), CDouble (..), CInt (..))
import OpenCascade.Internal.Bool (boolToCBool)
import Data.Acquire (Acquire, mkAcquire)
import Data.Coerce (coerce)

foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_new_BRepBuilderAPI_Sewing" rawNew :: CDouble -> CBool -> CBool -> CBool -> CBool -> IO (Ptr Sewing)

new :: Double -> Bool -> Bool -> Bool -> Bool -> Acquire (Ptr Sewing)
new tolerance opt1 opt2 opt3 opt4 = mkAcquire (rawNew (coerce tolerance) (boolToCBool opt1) (boolToCBool opt2) (boolToCBool opt3) (boolToCBool opt4)) deleteSewing

foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_BRepBuilderAPI_Sewing_load" rawLoad
    :: Ptr Sewing
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

load :: Ptr Sewing -> Ptr TopoDS.Shape -> IO ()
load sewing shape = wrapException $ rawLoad sewing shape

foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_BRepBuilderAPI_Sewing_add" rawAdd
    :: Ptr Sewing
    -> Ptr TopoDS.Shape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

add :: Ptr Sewing -> Ptr TopoDS.Shape -> IO ()
add sewing shape = wrapException $ rawAdd sewing shape

foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_BRepBuilderAPI_Sewing_perform" rawPerform
    :: Ptr Sewing
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

perform :: Ptr Sewing -> IO ()
perform sewing = wrapException $ rawPerform sewing

foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_BRepBuilderAPI_Sewing_sewedShape" rawSewedShape
    :: Ptr Sewing
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Shape)

sewedShape :: Ptr Sewing -> Acquire (Ptr TopoDS.Shape)
sewedShape sewing = mkAcquire (wrapException $ rawSewedShape sewing) (deleteShape)

foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_BRepBuilderAPI_Sewing_nbFreeEdges" rawNbFreeEdges:: Ptr Sewing -> IO (CInt)

nbFreeEdges :: Ptr Sewing -> IO Int
nbFreeEdges = fmap fromIntegral <$> rawNbFreeEdges


foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_BRepBuilderAPI_Sewing_nbContigousEdges" rawNbContigousEdges:: Ptr Sewing -> IO (CInt)

nbContigousEdges :: Ptr Sewing -> IO Int
nbContigousEdges = fmap fromIntegral <$> rawNbContigousEdges


foreign import capi unsafe "hs_BRepBuilderAPI_Sewing.h hs_BRepBuilderAPI_Sewing_nbMultipleEdges" rawNbMultipleEdges:: Ptr Sewing -> IO (CInt)

nbMultipleEdges :: Ptr Sewing -> IO Int
nbMultipleEdges = fmap fromIntegral <$> rawNbMultipleEdges