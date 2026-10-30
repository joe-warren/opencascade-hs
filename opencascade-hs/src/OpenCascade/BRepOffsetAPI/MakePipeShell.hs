{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.MakePipeShell
( MakePipeShell
, new
, setModeFrenet
, setDiscreteMode
, add
, makeSolid
, isDone
) where

import OpenCascade.BRepOffsetAPI.Types (MakePipeShell)
import OpenCascade.BRepOffsetAPI.Internal.Destructors (deleteMakePipeShell)
import qualified OpenCascade.TopoDS as TopoDS
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.Ptr
import Foreign.C (CBool (..), CInt (..))
import Data.Acquire

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_new_BRepOffsetAPI_MakePipeShell" rawNew :: Ptr TopoDS.Wire -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr MakePipeShell)

new :: Ptr TopoDS.Wire -> Acquire (Ptr MakePipeShell)
new spine = mkAcquire (wrapException $ rawNew spine) deleteMakePipeShell

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_setModeFrenet" rawSetModeFrenet :: Ptr MakePipeShell -> CBool -> IO ()

setModeFrenet :: Ptr MakePipeShell -> Bool -> IO ()
setModeFrenet builder isFrenet = rawSetModeFrenet builder (boolToCBool isFrenet)

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_setDiscreteMode" setDiscreteMode :: Ptr MakePipeShell -> IO ()

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_add" rawAdd :: Ptr MakePipeShell -> Ptr TopoDS.Wire -> CBool -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

add :: Ptr MakePipeShell -> Ptr TopoDS.Wire -> Bool -> Bool -> IO ()
add builder profile withContact withCorrection =
    wrapException $ rawAdd builder profile (boolToCBool withContact) (boolToCBool withCorrection)

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_makeSolid" rawMakeSolid :: Ptr MakePipeShell -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

makeSolid :: Ptr MakePipeShell -> IO Bool
makeSolid builder = cBoolToBool <$> wrapException (rawMakeSolid builder)

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_isDone" rawIsDone :: Ptr MakePipeShell -> IO CBool

isDone :: Ptr MakePipeShell -> IO Bool
isDone = fmap cBoolToBool . rawIsDone
