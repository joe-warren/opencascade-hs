{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.MakeOffset
( MakeOffset
, new
, fromFace
, init
, addWire
, perform
) where

import Prelude hiding (init)
import OpenCascade.BRepOffsetAPI.Types (MakeOffset)
import Data.Acquire
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteMakeOffset)
import Foreign (Ptr)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Data.Coerce (coerce)
import qualified OpenCascade.GeomAbs.JoinType as GeomAbs.JoinType
import OpenCascade.Internal.Bool (boolToCBool)
import OpenCascade.Internal.Exception (wrapException)

foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffset.h hs_new_BRepOffsetAPI_MakeOffset" rawNew :: IO (Ptr MakeOffset)

new :: Acquire (Ptr MakeOffset)
new = mkAcquire rawNew deleteMakeOffset

foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffset.h hs_new_BRepOffsetAPI_MakeOffset_fromFace" rawFromFace 
    :: Ptr TopoDS.Face 
    -> CInt
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr MakeOffset)

fromFace :: Ptr TopoDS.Face -> GeomAbs.JoinType.JoinType -> Bool -> Acquire (Ptr MakeOffset)
fromFace face join isOpen = 
    mkAcquire (
        wrapException (
            rawFromFace face (fromIntegral $ fromEnum join) (boolToCBool isOpen)
        )
    ) 
    deleteMakeOffset

    
foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffset.h hs_BRepOffsetAPI_MakeOffset_Init" rawInit 
    :: Ptr MakeOffset
    -> Ptr TopoDS.Face 
    -> CInt
    -> CBool
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

init :: Ptr MakeOffset -> Ptr TopoDS.Face -> GeomAbs.JoinType.JoinType -> Bool -> IO ()
init builder face join isOpen = 
        wrapException (
            rawInit builder face (fromIntegral $ fromEnum join) (boolToCBool isOpen)
        )

foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffset.h hs_BRepOffsetAPI_MakeOffset_AddWire" rawAddWire
    :: Ptr MakeOffset
    -> Ptr TopoDS.Wire
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addWire :: Ptr MakeOffset -> Ptr TopoDS.Wire -> IO ()
addWire builder wire = wrapException (rawAddWire builder wire)

foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffset.h hs_BRepOffsetAPI_MakeOffset_Perform" rawPerform
    :: Ptr MakeOffset
    -> CDouble
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

perform :: Ptr MakeOffset -> Double -> Double -> IO ()
perform builder amount alt = 
        wrapException (
            (coerce rawPerform) builder amount alt
        )


