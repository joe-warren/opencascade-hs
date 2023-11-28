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
, isEqual
, isPartner
, isSame
, isNotEqual
, emptyCopy
, emptyCopied
, hashCode
) where

import Prelude hiding (reverse)
import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Destructors
import OpenCascade.Internal.Bool
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

foreign import capi unsafe "hs_TopoDS_Shape.h hs_new_TopoDS_Shape" rawCopy :: Ptr Shape -> IO (Ptr Shape)

copy :: Ptr Shape -> Acquire (Ptr Shape)
copy shape = mkAcquire (rawCopy shape)  deleteShape

-- isNull 
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsNull" rawIsNull :: Ptr Shape -> IO CBool

isNull :: Ptr Shape -> IO Bool
isNull s = cBoolToBool <$> rawIsNull s

-- Nullify 
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Nullify" nullify :: Ptr Shape -> IO ()

-- location 
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Location" rawLocation :: Ptr Shape -> IO (Ptr TopLoc.Location)

location :: Ptr Shape -> Acquire (Ptr TopLoc.Location)
location s = mkAcquire (rawLocation s) TopLoc.Destructors.deleteLocation  

-- setLocation 
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetLocation" setLocation :: Ptr Shape -> Ptr TopLoc.Location -> IO ()

-- located
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Located" rawLocated :: Ptr Shape -> Ptr TopLoc.Location -> IO (Ptr Shape)

located :: Ptr Shape -> Ptr TopLoc.Location -> Acquire (Ptr Shape)
located s l = mkAcquire (rawLocated s l) deleteShape


-- orientation

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Orientation" rawOrientation :: Ptr Shape -> IO (CInt)

orientation :: Ptr Shape -> IO TopAbs.Orientation
orientation s = toEnum . fromIntegral <$> rawOrientation s

-- setOrientation

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetOrientation" rawSetOrientation :: Ptr Shape -> CInt -> IO ()

setOrientation :: Ptr Shape -> TopAbs.Orientation -> IO ()
setOrientation s o = rawSetOrientation s (fromIntegral . fromEnum $ o) 


-- oriented
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Oriented" rawOriented :: Ptr Shape -> CInt -> IO (Ptr Shape)

oriented :: Ptr Shape -> TopAbs.Orientation -> Acquire (Ptr Shape)
oriented s o = mkAcquire (rawOriented s (fromIntegral . fromEnum $ o)) deleteShape

-- shapeType 
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_ShapeType" rawShapeType :: Ptr Shape -> IO CInt

shapeType :: Ptr Shape -> IO TopAbs.ShapeEnum
shapeType s = toEnum . fromIntegral <$> rawShapeType s

-- free

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Free" rawFree :: Ptr Shape -> IO CBool

free :: Ptr Shape -> IO Bool
free s = cBoolToBool <$> rawFree s

--setFree 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetFree" rawSetFree :: Ptr Shape -> CBool-> IO ()

setFree :: Ptr Shape -> Bool -> IO ()
setFree s b = rawSetFree s (boolToCBool b)

-- locked

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Locked" rawLocked :: Ptr Shape -> IO CBool

locked :: Ptr Shape -> IO Bool
locked s = cBoolToBool <$> rawLocked s

--setLocked 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetLocked" rawSetLocked :: Ptr Shape -> CBool-> IO ()

setLocked :: Ptr Shape -> Bool -> IO ()
setLocked s b = rawSetLocked s (boolToCBool b)



-- modified

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Modified" rawModified :: Ptr Shape -> IO CBool

modified :: Ptr Shape -> IO Bool
modified s = cBoolToBool <$> rawModified s

--setModified 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetModified" rawSetModified :: Ptr Shape -> CBool-> IO ()

setModified :: Ptr Shape -> Bool -> IO ()
setModified s b = rawSetModified s (boolToCBool b)


-- checked

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Checked" rawChecked :: Ptr Shape -> IO CBool

checked :: Ptr Shape -> IO Bool
checked s = cBoolToBool <$> rawChecked s

--setChecked 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetChecked" rawSetChecked :: Ptr Shape -> CBool-> IO ()

setChecked :: Ptr Shape -> Bool -> IO ()
setChecked s b = rawSetChecked s (boolToCBool b)

-- orientable

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Orientable" rawOrientable :: Ptr Shape -> IO CBool

orientable :: Ptr Shape -> IO Bool
orientable s = cBoolToBool <$> rawOrientable s

--setOrientable 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetOrientable" rawSetOrientable :: Ptr Shape -> CBool-> IO ()

setOrientable :: Ptr Shape -> Bool -> IO ()
setOrientable s b = rawSetOrientable s (boolToCBool b)

-- closed

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Closed" rawClosed :: Ptr Shape -> IO CBool

closed :: Ptr Shape -> IO Bool
closed s = cBoolToBool <$> rawClosed s

--setClosed 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetClosed" rawSetClosed :: Ptr Shape -> CBool-> IO ()

setClosed :: Ptr Shape -> Bool -> IO ()
setClosed s b = rawSetClosed s (boolToCBool b)


-- infinite

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Infinite" rawInfinite :: Ptr Shape -> IO CBool

infinite :: Ptr Shape -> IO Bool
infinite s = cBoolToBool <$> rawInfinite s

--setInfinite 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetInfinite" rawSetInfinite :: Ptr Shape -> CBool-> IO ()

setInfinite :: Ptr Shape -> Bool -> IO ()
setInfinite s b = rawSetInfinite s (boolToCBool b)



-- convex

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Convex" rawConvex :: Ptr Shape -> IO CBool

convex :: Ptr Shape -> IO Bool
convex s = cBoolToBool <$> rawConvex s

--setConvex 

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_SetConvex" rawSetConvex :: Ptr Shape -> CBool-> IO ()

setConvex :: Ptr Shape -> Bool -> IO ()
setConvex s b = rawSetConvex s (boolToCBool b)


-- move
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Move" move :: Ptr Shape -> Ptr TopLoc.Location -> IO ()

-- moved
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Moved" rawMoved :: Ptr Shape -> Ptr TopLoc.Location -> IO (Ptr Shape)

moved :: Ptr Shape -> Ptr TopLoc.Location -> Acquire (Ptr Shape)
moved s l = mkAcquire (rawMoved s l) deleteShape

-- nbChildren

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_NbChildren" rawNbChildren :: Ptr Shape -> IO (CInt)

nbChildren :: Ptr Shape -> IO Int
nbChildren s = fromIntegral <$> rawNbChildren s

-- reverse
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Reverse" reverse :: Ptr Shape -> IO ()

-- reversed
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Reversed" rawReversed :: Ptr Shape -> IO (Ptr Shape)

reversed :: Ptr Shape -> Acquire (Ptr Shape)
reversed s = mkAcquire (rawReversed s) deleteShape


-- complement
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Complement" complement :: Ptr Shape -> IO ()

-- complemented
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_Complemented" rawComplemented :: Ptr Shape -> IO (Ptr Shape)

complemented :: Ptr Shape -> Acquire (Ptr Shape)
complemented s = mkAcquire (rawComplemented s) deleteShape

-- isEqual

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsEqual" rawIsEqual :: Ptr Shape -> Ptr Shape -> IO CBool

isEqual :: Ptr Shape -> Ptr Shape -> IO Bool
isEqual a b = cBoolToBool <$> rawIsEqual a b


-- isSame

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsSame" rawIsSame :: Ptr Shape -> Ptr Shape -> IO CBool

isSame :: Ptr Shape -> Ptr Shape -> IO Bool
isSame a b = cBoolToBool <$> rawIsSame a b


-- isPartner

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsPartner" rawIsPartner :: Ptr Shape -> Ptr Shape -> IO CBool

isPartner :: Ptr Shape -> Ptr Shape -> IO Bool
isPartner a b = cBoolToBool <$> rawIsPartner a b

-- isNotEqual

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_IsNotEqual" rawIsNotEqual :: Ptr Shape -> Ptr Shape -> IO CBool

isNotEqual :: Ptr Shape -> Ptr Shape -> IO Bool
isNotEqual a b = cBoolToBool <$> rawIsNotEqual a b

-- emptyCopy
--

foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_EmptyCopy" emptyCopy :: Ptr Shape -> IO ()

-- emptyCopied
--
foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_EmptyCopied" rawEmptyCopied :: Ptr Shape -> IO (Ptr Shape)

emptyCopied :: Ptr Shape -> Acquire (Ptr Shape)
emptyCopied s = mkAcquire (rawEmptyCopied s) deleteShape


foreign import capi unsafe "hs_TopoDS_Shape.h hs_TopoDS_Shape_hashCode" rawHashCode :: Ptr Shape -> CInt -> IO CInt

hashCode :: Ptr Shape -> Int -> IO Int
hashCode shape upperBound = fromIntegral <$> rawHashCode shape (fromIntegral upperBound) 
