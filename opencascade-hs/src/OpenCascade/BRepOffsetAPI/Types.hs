{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepOffsetAPI.Types
( MakePipe
, MakeOffsetShape
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.MakeShape (MakeShape)

data MakePipe
data MakeOffsetShape

instance Inheritance.SubTypeOf MakeShape MakePipe
instance Inheritance.SubTypeOf MakeShape MakeOffsetShape