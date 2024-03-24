{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpenCascade.RWGltf.Types
( CafWriter
, CafReader
) where

import OpenCascade.Inheritance (SubTypeOf)
import qualified OpenCascade.RWMesh.Types as RWMesh

data CafWriter
data CafReader

instance SubTypeOf RWMesh.CafReader CafReader