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
) where

import Prelude hiding (reverse)
import OpenCascade.TopoDS.Types
import OpenCascade.TopoDS.Internal.Context
import OpenCascade.TopoDS.Internal.Destructors
import OpenCascade.Internal.Bool (boolToCBool, cBoolToBool)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

import qualified OpenCascade.TopLoc as TopLoc
import qualified OpenCascade.TopLoc.Internal.Destructors as TopLoc.Destructors
import qualified OpenCascade.TopAbs as TopAbs

C.context (C.cppCtx <> topoDSContext)

C.include "<TopoDS_Shape.hxx>"
C.include "<TopLoc_Location.hxx>"
C.include "<TopAbs_Orientation.hxx>"
-- new

new :: Acquire (Ptr Shape)
new = mkAcquire createShape deleteShape
  where
    createShape = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape();
    } |]

-- copy

copy :: Ptr Shape -> Acquire (Ptr Shape)
copy shape = mkAcquire createCopy deleteShape
  where
    createCopy = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape(*$(TopoDS_Shape* shape));
    } |]

-- isNull 

isNull :: Ptr Shape -> IO Bool
isNull shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->IsNull();
  } |]
  return (cBoolToBool result)

-- Nullify 

nullify :: Ptr Shape -> IO ()
nullify shape = [C.throwBlock| void {
  $(TopoDS_Shape* shape)->Nullify();
} |]

-- location 

location :: Ptr Shape -> Acquire (Ptr TopLoc.Location)
location shape = mkAcquire createLocation TopLoc.Destructors.deleteLocation  
  where
    createLocation = [C.throwBlock| TopLoc_Location* {
      return new TopLoc_Location($(TopoDS_Shape* shape)->Location());
    } |]

-- setLocation 

setLocation :: Ptr Shape -> Ptr TopLoc.Location -> IO ()
setLocation shape loc = [C.throwBlock| void {
  $(TopoDS_Shape* shape)->Location(*$(TopLoc_Location* loc));
} |]

-- located

located :: Ptr Shape -> Ptr TopLoc.Location -> Acquire (Ptr Shape)
located shape loc = mkAcquire createLocated deleteShape
  where
    createLocated = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(TopoDS_Shape* shape)->Located(*$(TopLoc_Location* loc)));
    } |]


-- orientation

orientation :: Ptr Shape -> IO TopAbs.Orientation
orientation shape = do
  result <- [C.throwBlock| int {
    return static_cast<int>($(TopoDS_Shape* shape)->Orientation());
  } |]
  return (toEnum . fromIntegral $ result)

-- setOrientation

setOrientation :: Ptr Shape -> TopAbs.Orientation -> IO ()
setOrientation shape orient = 
  let cOrient = fromIntegral . fromEnum $ orient
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Orientation(static_cast<TopAbs_Orientation>($(int cOrient)));
  } |]

-- oriented

oriented :: Ptr Shape -> TopAbs.Orientation -> Acquire (Ptr Shape)
oriented shape orient = mkAcquire createOriented deleteShape
  where
    createOriented = 
      let cOrient = fromIntegral . fromEnum $ orient
      in [C.throwBlock| TopoDS_Shape* {
        return new TopoDS_Shape($(TopoDS_Shape* shape)->Oriented(static_cast<TopAbs_Orientation>($(int cOrient))));
      } |]

-- shapeType 

shapeType :: Ptr Shape -> IO TopAbs.ShapeEnum
shapeType shape = do
  result <- [C.throwBlock| int {
    return static_cast<int>($(TopoDS_Shape* shape)->ShapeType());
  } |]
  return (toEnum . fromIntegral $ result)

-- free

free :: Ptr Shape -> IO Bool
free shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Free();
  } |]
  return (cBoolToBool result)

-- setFree 

setFree :: Ptr Shape -> Bool -> IO ()
setFree shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Free($(bool cValue));
  } |]

-- locked

locked :: Ptr Shape -> IO Bool
locked shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Locked();
  } |]
  return (cBoolToBool result)

-- setLocked 

setLocked :: Ptr Shape -> Bool -> IO ()
setLocked shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Locked($(bool cValue));
  } |]

-- modified

modified :: Ptr Shape -> IO Bool
modified shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Modified();
  } |]
  return (cBoolToBool result)

-- setModified 

setModified :: Ptr Shape -> Bool -> IO ()
setModified shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Modified($(bool cValue));
  } |]


-- checked

checked :: Ptr Shape -> IO Bool
checked shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Checked();
  } |]
  return (cBoolToBool result)

-- setChecked 

setChecked :: Ptr Shape -> Bool -> IO ()
setChecked shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Checked($(bool cValue));
  } |]

-- orientable

orientable :: Ptr Shape -> IO Bool
orientable shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Orientable();
  } |]
  return (cBoolToBool result)

-- setOrientable 

setOrientable :: Ptr Shape -> Bool -> IO ()
setOrientable shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Orientable($(bool cValue));
  } |]

-- closed

closed :: Ptr Shape -> IO Bool
closed shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Closed();
  } |]
  return (cBoolToBool result)

-- setClosed 

setClosed :: Ptr Shape -> Bool -> IO ()
setClosed shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Closed($(bool cValue));
  } |]

-- infinite

infinite :: Ptr Shape -> IO Bool
infinite shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Infinite();
  } |]
  return (cBoolToBool result)

-- setInfinite 

setInfinite :: Ptr Shape -> Bool -> IO ()
setInfinite shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Infinite($(bool cValue));
  } |]

-- convex

convex :: Ptr Shape -> IO Bool
convex shape = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shape)->Convex();
  } |]
  return (cBoolToBool result)

-- setConvex 

setConvex :: Ptr Shape -> Bool -> IO ()
setConvex shape value = 
  let cValue = boolToCBool value
  in [C.throwBlock| void {
    $(TopoDS_Shape* shape)->Convex($(bool cValue));
  } |]


-- move

move :: Ptr Shape -> Ptr TopLoc.Location -> IO ()
move shape loc = [C.throwBlock| void {
  $(TopoDS_Shape* shape)->Move(*$(TopLoc_Location* loc));
} |]

-- moved

moved :: Ptr Shape -> Ptr TopLoc.Location -> Acquire (Ptr Shape)
moved shape loc = mkAcquire createMoved deleteShape
  where
    createMoved = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(TopoDS_Shape* shape)->Moved(*$(TopLoc_Location* loc)));
    } |]

-- nbChildren

nbChildren :: Ptr Shape -> IO Int
nbChildren shape = do
  result <- [C.throwBlock| int {
    return $(TopoDS_Shape* shape)->NbChildren();
  } |]
  return (fromIntegral result)

-- reverse

reverse :: Ptr Shape -> IO ()
reverse shape = [C.throwBlock| void {
  $(TopoDS_Shape* shape)->Reverse();
} |]

-- reversed

reversed :: Ptr Shape -> Acquire (Ptr Shape)
reversed shape = mkAcquire createReversed deleteShape
  where
    createReversed = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(TopoDS_Shape* shape)->Reversed());
    } |]

-- complement

complement :: Ptr Shape -> IO ()
complement shape = [C.throwBlock| void {
  $(TopoDS_Shape* shape)->Complement();
} |]

-- complemented

complemented :: Ptr Shape -> Acquire (Ptr Shape)
complemented shape = mkAcquire createComplemented deleteShape
  where
    createComplemented = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(TopoDS_Shape* shape)->Complemented());
    } |]

-- isEqual

isEqual :: Ptr Shape -> Ptr Shape -> IO Bool
isEqual shapeA shapeB = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shapeA)->IsEqual(*$(TopoDS_Shape* shapeB));
  } |]
  return (cBoolToBool result)

-- isSame

isSame :: Ptr Shape -> Ptr Shape -> IO Bool
isSame shapeA shapeB = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shapeA)->IsSame(*$(TopoDS_Shape* shapeB));
  } |]
  return (cBoolToBool result)

-- isPartner

isPartner :: Ptr Shape -> Ptr Shape -> IO Bool
isPartner shapeA shapeB = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shapeA)->IsPartner(*$(TopoDS_Shape* shapeB));
  } |]
  return (cBoolToBool result)

-- isNotEqual

isNotEqual :: Ptr Shape -> Ptr Shape -> IO Bool
isNotEqual shapeA shapeB = do
  result <- [C.throwBlock| bool {
    return $(TopoDS_Shape* shapeA)->IsNotEqual(*$(TopoDS_Shape* shapeB));
  } |]
  return (cBoolToBool result)

-- emptyCopy

emptyCopy :: Ptr Shape -> IO ()
emptyCopy shape = [C.throwBlock| void {
  $(TopoDS_Shape* shape)->EmptyCopy();
} |]

-- emptyCopied

emptyCopied :: Ptr Shape -> Acquire (Ptr Shape)
emptyCopied shape = mkAcquire createEmptyCopied deleteShape
  where
    createEmptyCopied = [C.throwBlock| TopoDS_Shape* {
      return new TopoDS_Shape($(TopoDS_Shape* shape)->EmptyCopied());
    } |]
