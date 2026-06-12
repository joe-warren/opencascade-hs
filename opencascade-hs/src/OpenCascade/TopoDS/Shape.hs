{-# LANGUAGE CApiFFI #-}
module OpenCascade.TopoDS.Shape
( Shape
, new
, copy
, isNull
, nullify
, location
, setLocation
, located
, orientation
, setOrientation
, oriented
, shapeType
, free
, setFree
, locked
, setLocked
, modified
, setModified
, checked
, setChecked
, orientable
, setOrientable
, closed
, setClosed
, infinite
, setInfinite
, convex
, setConvex
, move
, moved
, nbChildren
, reverse
, reversed
, complement
, complemented
, compose
, composed
, isEqual
, isPartner
, isSame
, isNotEqual
, emptyCopy
, emptyCopied
) where

import Prelude hiding (reverse)
import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import OpenCascade.Internal.Bool
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire

import qualified OpenCascade.TopLoc as TopLoc
import qualified OpenCascade.TopLoc.Internal.Destructors as TopLoc.Destructors
import qualified OpenCascade.TopAbs as TopAbs
-- new

foreign import capi unsafe "hs_TopoDS_Shape.h hs_new_TopoDS_Shape" rawNew :: IO (Ptr Shape)

new :: Acquire (Ptr Shape)
new = mkAcquire rawNew  deleteShape

-- copy

foreign import capi unsafe "hs_TopoDS_Shape.h hs_new_TopoDS_Shape_copy" rawCopy :: Ptr Shape -> IO (Ptr Shape)

copy :: Ptr Shape -> Acquire (Ptr Shape)
copy shape = mkAcquire (rawCopy shape)  deleteShape

-- isNull
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsNull" rawIsNull :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

isNull :: Ptr Shape -> IO Bool
isNull s = cBoolToBool <$> wrapException (rawIsNull s)

-- Nullify
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Nullify" rawNullify :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

nullify :: Ptr Shape -> IO ()
nullify s = wrapException $ rawNullify s

-- location
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Location" rawLocation :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr TopLoc.Location)

location :: Ptr Shape -> Acquire (Ptr TopLoc.Location)
location s = mkAcquire (wrapException $ rawLocation s) TopLoc.Destructors.deleteLocation

-- setLocation
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetLocation" rawSetLocation :: Ptr Shape -> Ptr TopLoc.Location -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setLocation :: Ptr Shape -> Ptr TopLoc.Location -> IO ()
setLocation s l = wrapException $ rawSetLocation s l

-- located
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Located" rawLocated :: Ptr Shape -> Ptr TopLoc.Location -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Shape)

located :: Ptr Shape -> Ptr TopLoc.Location -> Acquire (Ptr Shape)
located s l = mkAcquire (wrapException $ rawLocated s l) deleteShape


-- orientation

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Orientation" rawOrientation :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO (CInt)

orientation :: Ptr Shape -> IO TopAbs.Orientation
orientation s = toEnum . fromIntegral <$> wrapException (rawOrientation s)

-- setOrientation

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetOrientation" rawSetOrientation :: Ptr Shape -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setOrientation :: Ptr Shape -> TopAbs.Orientation -> IO ()
setOrientation s o = wrapException $ rawSetOrientation s (fromIntegral . fromEnum $ o)


-- oriented
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Oriented" rawOriented :: Ptr Shape -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Shape)

oriented :: Ptr Shape -> TopAbs.Orientation -> Acquire (Ptr Shape)
oriented s o = mkAcquire (wrapException $ rawOriented s (fromIntegral . fromEnum $ o)) deleteShape

-- shapeType
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_ShapeType" rawShapeType :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CInt

shapeType :: Ptr Shape -> IO TopAbs.ShapeEnum
shapeType s = toEnum . fromIntegral <$> wrapException (rawShapeType s)

-- free

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Free" rawFree :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

free :: Ptr Shape -> IO Bool
free s = cBoolToBool <$> wrapException (rawFree s)

--setFree

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetFree" rawSetFree :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setFree :: Ptr Shape -> Bool -> IO ()
setFree s b = wrapException $ rawSetFree s (boolToCBool b)

-- locked

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Locked" rawLocked :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

locked :: Ptr Shape -> IO Bool
locked s = cBoolToBool <$> wrapException (rawLocked s)

--setLocked

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetLocked" rawSetLocked :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setLocked :: Ptr Shape -> Bool -> IO ()
setLocked s b = wrapException $ rawSetLocked s (boolToCBool b)



-- modified

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Modified" rawModified :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

modified :: Ptr Shape -> IO Bool
modified s = cBoolToBool <$> wrapException (rawModified s)

--setModified

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetModified" rawSetModified :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setModified :: Ptr Shape -> Bool -> IO ()
setModified s b = wrapException $ rawSetModified s (boolToCBool b)


-- checked

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Checked" rawChecked :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

checked :: Ptr Shape -> IO Bool
checked s = cBoolToBool <$> wrapException (rawChecked s)

--setChecked

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetChecked" rawSetChecked :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setChecked :: Ptr Shape -> Bool -> IO ()
setChecked s b = wrapException $ rawSetChecked s (boolToCBool b)

-- orientable

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Orientable" rawOrientable :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

orientable :: Ptr Shape -> IO Bool
orientable s = cBoolToBool <$> wrapException (rawOrientable s)

--setOrientable

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetOrientable" rawSetOrientable :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setOrientable :: Ptr Shape -> Bool -> IO ()
setOrientable s b = wrapException $ rawSetOrientable s (boolToCBool b)

-- closed

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Closed" rawClosed :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

closed :: Ptr Shape -> IO Bool
closed s = cBoolToBool <$> wrapException (rawClosed s)

--setClosed

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetClosed" rawSetClosed :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setClosed :: Ptr Shape -> Bool -> IO ()
setClosed s b = wrapException $ rawSetClosed s (boolToCBool b)


-- infinite

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Infinite" rawInfinite :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

infinite :: Ptr Shape -> IO Bool
infinite s = cBoolToBool <$> wrapException (rawInfinite s)

--setInfinite

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetInfinite" rawSetInfinite :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setInfinite :: Ptr Shape -> Bool -> IO ()
setInfinite s b = wrapException $ rawSetInfinite s (boolToCBool b)



-- convex

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Convex" rawConvex :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

convex :: Ptr Shape -> IO Bool
convex s = cBoolToBool <$> wrapException (rawConvex s)

--setConvex

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetConvex" rawSetConvex :: Ptr Shape -> CBool -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

setConvex :: Ptr Shape -> Bool -> IO ()
setConvex s b = wrapException $ rawSetConvex s (boolToCBool b)


-- move
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Move" rawMove :: Ptr Shape -> Ptr TopLoc.Location -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

move :: Ptr Shape -> Ptr TopLoc.Location -> IO ()
move s l = wrapException $ rawMove s l

-- moved
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Moved" rawMoved :: Ptr Shape -> Ptr TopLoc.Location -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Shape)

moved :: Ptr Shape -> Ptr TopLoc.Location -> Acquire (Ptr Shape)
moved s l = mkAcquire (wrapException $ rawMoved s l) deleteShape

-- nbChildren

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_NbChildren" rawNbChildren :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO (CInt)

nbChildren :: Ptr Shape -> IO Int
nbChildren s = fromIntegral <$> wrapException (rawNbChildren s)

-- reverse
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Reverse" rawReverse :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

reverse :: Ptr Shape -> IO ()
reverse s = wrapException $ rawReverse s

-- reversed
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Reversed" rawReversed :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Shape)

reversed :: Ptr Shape -> Acquire (Ptr Shape)
reversed s = mkAcquire (wrapException $ rawReversed s) deleteShape


-- complement
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Complement" rawComplement :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

complement :: Ptr Shape -> IO ()
complement s = wrapException $ rawComplement s

-- complemented
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Complemented" rawComplemented :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Shape)

complemented :: Ptr Shape -> Acquire (Ptr Shape)
complemented s = mkAcquire (wrapException $ rawComplemented s) deleteShape

-- compose
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Compose" rawCompose :: Ptr Shape -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

compose :: Ptr Shape -> TopAbs.Orientation -> IO ()
compose s o = wrapException $ rawCompose s (fromIntegral . fromEnum $ o)

-- composed
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Composed" rawComposed :: Ptr Shape -> CInt -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Shape)

composed :: Ptr Shape -> TopAbs.Orientation -> Acquire (Ptr Shape)
composed s o = mkAcquire (wrapException $ rawComposed s (fromIntegral . fromEnum $ o)) deleteShape

-- isEqual

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsEqual" rawIsEqual :: Ptr Shape -> Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

isEqual :: Ptr Shape -> Ptr Shape -> IO Bool
isEqual a b = cBoolToBool <$> wrapException (rawIsEqual a b)


-- isSame

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsSame" rawIsSame :: Ptr Shape -> Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

isSame :: Ptr Shape -> Ptr Shape -> IO Bool
isSame a b = cBoolToBool <$> wrapException (rawIsSame a b)


-- isPartner

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsPartner" rawIsPartner :: Ptr Shape -> Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

isPartner :: Ptr Shape -> Ptr Shape -> IO Bool
isPartner a b = cBoolToBool <$> wrapException (rawIsPartner a b)

-- isNotEqual

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsNotEqual" rawIsNotEqual :: Ptr Shape -> Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO CBool

isNotEqual :: Ptr Shape -> Ptr Shape -> IO Bool
isNotEqual a b = cBoolToBool <$> wrapException (rawIsNotEqual a b)

-- emptyCopy
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_EmptyCopy" rawEmptyCopy :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO ()

emptyCopy :: Ptr Shape -> IO ()
emptyCopy s = wrapException $ rawEmptyCopy s

-- emptyCopied
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_EmptyCopied" rawEmptyCopied :: Ptr Shape -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr Shape)

emptyCopied :: Ptr Shape -> Acquire (Ptr Shape)
emptyCopied s = mkAcquire (wrapException $ rawEmptyCopied s) deleteShape
