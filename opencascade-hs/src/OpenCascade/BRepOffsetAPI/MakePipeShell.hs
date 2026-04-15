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
import Foreign.Ptr
import Foreign.C (CBool (..))
import Data.Acquire

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_new_BRepOffsetAPI_MakePipeShell" rawNew :: Ptr TopoDS.Wire -> IO (Ptr MakePipeShell)

new :: Ptr TopoDS.Wire -> Acquire (Ptr MakePipeShell)
new spine = mkAcquire (rawNew spine) deleteMakePipeShell

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_setModeFrenet" rawSetModeFrenet :: Ptr MakePipeShell -> CBool -> IO ()

-- | Set the sweep trihedron mode.
--
-- Pass 'True' for the classical Frenet trihedron, or 'False' for the
-- CorrectedFrenet trihedron (which handles inflection points without flipping).
setModeFrenet :: Ptr MakePipeShell -> Bool -> IO ()
setModeFrenet builder isFrenet = rawSetModeFrenet builder (boolToCBool isFrenet)

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_setDiscreteMode" setDiscreteMode :: Ptr MakePipeShell -> IO ()

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_add" rawAdd :: Ptr MakePipeShell -> Ptr TopoDS.Wire -> CBool -> CBool -> IO ()

-- | Add a profile section to the sweep.
--
-- @withContact@: if 'True', translate the profile so it touches the spine at the start.
-- @withCorrection@: if 'True', rotate the profile so its normal aligns with the spine's start tangent.
add :: Ptr MakePipeShell -> Ptr TopoDS.Wire -> Bool -> Bool -> IO ()
add builder profile withContact withCorrection =
    rawAdd builder profile (boolToCBool withContact) (boolToCBool withCorrection)

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_makeSolid" rawMakeSolid :: Ptr MakePipeShell -> IO CBool

makeSolid :: Ptr MakePipeShell -> IO Bool
makeSolid = fmap cBoolToBool . rawMakeSolid

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipeShell.h hs_BRepOffsetAPI_MakePipeShell_isDone" rawIsDone :: Ptr MakePipeShell -> IO CBool

isDone :: Ptr MakePipeShell -> IO Bool
isDone = fmap cBoolToBool . rawIsDone
