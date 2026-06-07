{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepBuilderAPI.MakeWire 
( MakeWire
, new 
, addEdge
, addWire
, addListOfShape
, wire
, vertex
, isDone
, error
) where

import Prelude hiding (error)
import OpenCascade.BRepBuilderAPI.Types
import OpenCascade.BRepBuilderAPI.Internal.Destructors
import OpenCascade.Inheritance
import OpenCascade.Internal.Bool
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Internal.Destructors as TopoDS.Destructors
import qualified OpenCascade.TopTools as TopTools
import OpenCascade.BRepBuilderAPI.WireError (WireError)
import OpenCascade.Internal.Exception (wrapException)
import Foreign.C
import Foreign.Ptr
import Data.Acquire 

-- new

foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_new_BRepBuilderAPI_MakeWire" rawNew :: IO (Ptr MakeWire)

new :: Acquire (Ptr MakeWire)
new = mkAcquire rawNew deleteMakeWire

-- addEdge

foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_BRepBuilderAPI_MakeWire_AddEdge" rawAddEdge
    :: Ptr MakeWire
    -> Ptr TopoDS.Edge
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addEdge :: Ptr MakeWire -> Ptr TopoDS.Edge -> IO ()
addEdge builder edge = wrapException $ rawAddEdge builder edge


-- addWire

foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_BRepBuilderAPI_MakeWire_AddWire" rawAddWire
    :: Ptr MakeWire
    -> Ptr TopoDS.Wire
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addWire :: Ptr MakeWire -> Ptr TopoDS.Wire -> IO ()
addWire builder theWire = wrapException $ rawAddWire builder theWire


-- addListOfShape

foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_BRepBuilderAPI_MakeWire_AddListOfShape" rawAddListOfShape
    :: Ptr MakeWire
    -> Ptr TopTools.ListOfShape
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO ()

addListOfShape :: Ptr MakeWire -> Ptr TopTools.ListOfShape -> IO ()
addListOfShape builder list = wrapException $ rawAddListOfShape builder list

-- wire
--
foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_BRepBuilderAPI_MakeWire_Wire" rawWire
    :: Ptr MakeWire
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Wire)

wire :: Ptr MakeWire -> Acquire (Ptr TopoDS.Wire)
wire builder = mkAcquire (wrapException $ rawWire builder) (TopoDS.Destructors.deleteShape . upcast)

-- vertex
--
foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_BRepBuilderAPI_MakeWire_Vertex" rawVertex
    :: Ptr MakeWire
    -> Ptr CInt
    -> Ptr (Ptr ())
    -> IO (Ptr TopoDS.Vertex)

vertex :: Ptr MakeWire -> Acquire (Ptr TopoDS.Vertex)
vertex builder = mkAcquire (wrapException $ rawVertex builder) (TopoDS.Destructors.deleteShape . upcast)

-- isDone
--
foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_BRepBuilderAPI_MakeWire_IsDone" rawIsDone :: Ptr MakeWire -> IO (CBool)

isDone :: Ptr MakeWire -> IO Bool
isDone p = cBoolToBool <$> rawIsDone p

-- error
--
foreign import capi unsafe "hs_BRepBuilderAPI_MakeWire.h hs_BRepBuilderAPI_MakeWire_Error" rawError :: Ptr MakeWire -> IO (CInt)

error :: Ptr MakeWire -> IO WireError 
error p = toEnum . fromIntegral <$> rawError p
