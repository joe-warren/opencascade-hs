{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepPrimAPI.Types
( MakeBox
, MakeRevol
, MakeSphere
, MakeCylinder
, MakeCone
, MakePrism
) where

import OpenCascade.BRepBuilderAPI.MakeShape
import qualified OpenCascade.Inheritance as Inheritance

data MakeBox
data MakeRevol
data MakeSphere
data MakeCylinder
data MakeCone
data MakePrism

instance Inheritance.SubTypeOf MakeShape MakeBox
instance Inheritance.SubTypeOf MakeShape MakeRevol
instance Inheritance.SubTypeOf MakeShape MakeSphere
instance Inheritance.SubTypeOf MakeShape MakeCylinder
instance Inheritance.SubTypeOf MakeShape MakeCone
instance Inheritance.SubTypeOf MakeShape MakePrism