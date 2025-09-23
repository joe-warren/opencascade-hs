{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpenCascade.TopoDS.Types
( Shape
, CompSolid
, Compound
, Edge
, Face
, Shell
, Solid
, Vertex
, Wire
, Builder
)

where

import OpenCascade.Inheritance
import OpenCascade.TopAbs.ShapeEnum
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Foreign.Ptr
import Foreign.C

data Shape
data CompSolid
data Compound
data Edge
data Face
data Shell
data Solid
data Vertex
data Wire

data Builder

C.context (C.cppCtx <> C.cppTypePairs
    [ ("TopoDS_Shape", [t| Shape |])
    ])

C.include "<TopoDS_Shape.hxx>"
C.include "<TopAbs_ShapeEnum.hxx>"


-- duplicate definition of shape type from TopoDS.Shape
-- to simultaneously avoid Orphan Instances + circular dependencies
shapeType :: Ptr Shape -> IO ShapeEnum
shapeType shape = do
  result <- [C.throwBlock| int {
    TopoDS_Shape* cShape = static_cast<TopoDS_Shape*>($(void* rawShape));
    return static_cast<int>($(TopoDS_Shape* shape)->ShapeType());
  } |]
  return (toEnum . fromIntegral $ result)
  where
    rawShape = castPtr shape

enumDowncast :: ShapeEnum -> Ptr Shape -> IO (Maybe (Ptr t))
enumDowncast enum p = do
    e <- shapeType p
    return $ if e == enum 
                then Just (castPtr p)
                else Nothing


instance SubTypeOf Shape Compound

instance DiscriminatedSubTypeOf Shape Compound where
    downcast = enumDowncast ShapeEnum.Compound


instance SubTypeOf Shape CompSolid

instance DiscriminatedSubTypeOf Shape CompSolid where
    downcast = enumDowncast ShapeEnum.CompSolid


instance SubTypeOf Shape Solid

instance DiscriminatedSubTypeOf Shape Solid where
    downcast = enumDowncast ShapeEnum.Solid


instance SubTypeOf Shape Shell

instance DiscriminatedSubTypeOf Shape Shell where
    downcast = enumDowncast ShapeEnum.Shell


instance SubTypeOf Shape Face

instance DiscriminatedSubTypeOf Shape Face where
    downcast = enumDowncast ShapeEnum.Face

instance SubTypeOf Shape Wire

instance DiscriminatedSubTypeOf Shape Wire where
    downcast = enumDowncast ShapeEnum.Wire


instance SubTypeOf Shape Edge

instance DiscriminatedSubTypeOf Shape Edge where
    downcast = enumDowncast ShapeEnum.Edge


instance SubTypeOf Shape Vertex

instance DiscriminatedSubTypeOf Shape Vertex where
    downcast = enumDowncast ShapeEnum.Vertex

