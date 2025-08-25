{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BRepFilletAPI.Types
( MakeFillet
, MakeChamfer
) where

import qualified OpenCascade.Inheritance as Inheritance
import OpenCascade.BRepBuilderAPI.MakeShape (MakeShape)

data MakeFillet
data MakeChamfer

instance Inheritance.SubTypeOf MakeShape MakeFillet
instance Inheritance.SubTypeOf MakeShape MakeChamfer