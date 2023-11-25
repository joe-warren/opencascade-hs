{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepPrimAPI.Types
( MakeBox
, MakeRevol
) where

import OpenCascade.BRepBuilderAPI.MakeShape
import qualified OpenCascade.Inheritance as Inheritance

data MakeBox
data MakeRevol

instance Inheritance.SubTypeOf MakeShape MakeBox
instance Inheritance.SubTypeOf MakeShape MakeRevol