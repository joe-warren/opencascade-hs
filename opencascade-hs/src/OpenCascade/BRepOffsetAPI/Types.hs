{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepOffsetAPI.Types
( MakePipe
, MakeOffsetShape
, ThruSections
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.MakeShape (MakeShape)

data MakePipe
data MakeOffsetShape
data ThruSections

instance Inheritance.SubTypeOf MakeShape MakePipe
instance Inheritance.SubTypeOf MakeShape MakeOffsetShape
instance Inheritance.SubTypeOf MakeShape ThruSections