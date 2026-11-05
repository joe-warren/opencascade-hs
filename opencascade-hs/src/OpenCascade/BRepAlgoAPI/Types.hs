{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepAlgoAPI.Types
( Fuse
, Cut
, Common
, Section
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.Types (MakeShape)

data Fuse
data Cut
data Common
data Section

instance Inheritance.SubTypeOf MakeShape Fuse
instance Inheritance.SubTypeOf MakeShape Cut
instance Inheritance.SubTypeOf MakeShape Common
instance Inheritance.SubTypeOf MakeShape Section
