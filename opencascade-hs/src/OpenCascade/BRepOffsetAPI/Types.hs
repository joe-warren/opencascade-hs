{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepOffsetAPI.Types
( MakePipe
, MakePipeShell
, MakeOffsetShape
, ThruSections
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.MakeShape (MakeShape)

data MakePipe
data MakePipeShell
data MakeOffsetShape
data ThruSections

instance Inheritance.SubTypeOf MakeShape MakePipe
instance Inheritance.SubTypeOf MakeShape MakePipeShell
instance Inheritance.SubTypeOf MakeShape MakeOffsetShape
instance Inheritance.SubTypeOf MakeShape ThruSections
