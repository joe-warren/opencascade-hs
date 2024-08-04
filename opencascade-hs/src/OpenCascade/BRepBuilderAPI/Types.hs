{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepBuilderAPI.Types 
( MakeVertex
, MakeWire
, MakeFace
, MakeSolid
, MakeShape
, Sewing
) where

import qualified OpenCascade.Inheritance as Inheritance

data MakeVertex
data MakeWire
data MakeFace
data MakeSolid

data MakeShape

data Sewing

instance Inheritance.SubTypeOf MakeShape MakeVertex
instance Inheritance.SubTypeOf MakeShape MakeWire
instance Inheritance.SubTypeOf MakeShape MakeSolid
instance Inheritance.SubTypeOf MakeShape MakeFace

