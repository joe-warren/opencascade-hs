{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpenCascade.TopoDS 
( module OpenCascade.TopoDS.Types
) where

import OpenCascade.Inheritance
import OpenCascade.TopoDS.Shape
import OpenCascade.TopoDS.Types
import OpenCascade.TopAbs.ShapeEnum
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import Foreign.Ptr
import Data.Proxy

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

