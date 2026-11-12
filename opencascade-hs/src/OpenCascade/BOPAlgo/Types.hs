{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module OpenCascade.BOPAlgo.Types
( Builder
, BOP
, CellsBuilder
) where

import qualified OpenCascade.Inheritance as Inheritance

data Builder
data BOP
data CellsBuilder

instance Inheritance.SubTypeOf Builder BOP
instance Inheritance.SubTypeOf Builder CellsBuilder
