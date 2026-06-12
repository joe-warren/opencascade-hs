{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepPrimAPI.Types
( MakeBox
, MakeRevol
, MakeTorus
) where

import OpenCascade.BRepBuilderAPI.MakeShape
import qualified OpenCascade.Inheritance as Inheritance

data MakeBox
data MakeRevol
data MakeTorus

instance Inheritance.SubTypeOf MakeShape MakeBox
instance Inheritance.SubTypeOf MakeShape MakeRevol
instance Inheritance.SubTypeOf MakeShape MakeTorus