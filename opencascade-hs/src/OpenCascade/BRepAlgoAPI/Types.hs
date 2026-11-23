{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepAlgoAPI.Types
( Fuse
, Cut
, Common
, Section
, BuilderAlgo
, BooleanOperation
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.Types (MakeShape)

data Fuse
data Cut
data Common
data Section
data BooleanOperation
data BuilderAlgo

instance Inheritance.SubTypeOf MakeShape Fuse
instance Inheritance.SubTypeOf MakeShape Cut
instance Inheritance.SubTypeOf MakeShape Common
instance Inheritance.SubTypeOf MakeShape Section
instance Inheritance.SubTypeOf BooleanOperation Fuse
instance Inheritance.SubTypeOf BooleanOperation Cut
instance Inheritance.SubTypeOf BooleanOperation Common
instance Inheritance.SubTypeOf BooleanOperation Section
instance Inheritance.SubTypeOf BuilderAlgo Fuse
instance Inheritance.SubTypeOf BuilderAlgo Cut
instance Inheritance.SubTypeOf BuilderAlgo Common
instance Inheritance.SubTypeOf BuilderAlgo Section
