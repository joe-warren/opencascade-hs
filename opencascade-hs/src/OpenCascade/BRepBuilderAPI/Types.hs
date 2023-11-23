{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepBuilderAPI.Types where

import qualified OpenCascade.Inheritance as Inheritance

data MakeWire
data MakeFace
data MakeSolid

data MakeShape

instance Inheritance.SubTypeOf MakeShape MakeWire
instance Inheritance.SubTypeOf MakeShape MakeSolid
instance Inheritance.SubTypeOf MakeShape MakeFace

