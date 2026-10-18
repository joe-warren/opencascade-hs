{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepAlgoAPI.Types
( Fuse
, Cut
, Common
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.Types (MakeShape)

data Fuse
data Cut
data Common

instance Inheritance.SubTypeOf MakeShape Fuse
instance Inheritance.SubTypeOf MakeShape Cut
instance Inheritance.SubTypeOf MakeShape Common
