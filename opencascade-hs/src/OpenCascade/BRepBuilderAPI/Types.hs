{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepBuilderAPI.Types 
( MakeVertex
, MakeWire
, MakeFace
, MakeSolid
, MakeShape
, Sewing
, Transform
, GTransform
) where

import qualified OpenCascade.Inheritance as Inheritance

data MakeVertex
data MakeWire
data MakeFace
data MakeSolid

data MakeShape

data Sewing

data Transform
data GTransform

instance Inheritance.SubTypeOf MakeShape MakeVertex
instance Inheritance.SubTypeOf MakeShape MakeWire
instance Inheritance.SubTypeOf MakeShape MakeSolid
instance Inheritance.SubTypeOf MakeShape MakeFace
instance Inheritance.SubTypeOf MakeShape Transform
instance Inheritance.SubTypeOf MakeShape GTransform
