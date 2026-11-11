{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepOffsetAPI.Types
( MakePipe
, MakePipeShell
, MakeOffset
, MakeOffsetShape
, ThruSections
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.MakeShape (MakeShape)

data MakePipe
data MakePipeShell
data MakeOffset
data MakeOffsetShape
data ThruSections

instance Inheritance.SubTypeOf MakeShape MakePipe
instance Inheritance.SubTypeOf MakeShape MakePipeShell
instance Inheritance.SubTypeOf MakeShape MakeOffset
instance Inheritance.SubTypeOf MakeShape MakeOffsetShape
instance Inheritance.SubTypeOf MakeShape ThruSections