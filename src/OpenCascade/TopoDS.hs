{-# LANGUAGE ScopedTypeVariables #-}
module OpenCascade.TopoDS 
( module OpenCascade.TopoDS.Types
, ShapeSubtype (..)
, upcast 
, downcast
, unsafeDowncast
) where

import OpenCascade.TopoDS.Shape
import OpenCascade.TopoDS.Types
import OpenCascade.TopAbs.ShapeEnum
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import Foreign.Ptr
import Data.Proxy

class ShapeSubtype t where
    subtypeEnum :: Proxy t -> ShapeEnum

-- This is somewhat safer than just calling castPtr without the type constraint
upcast :: ShapeSubtype t => Ptr t -> Ptr Shape
upcast = castPtr

downcast :: forall t. ShapeSubtype t => Ptr Shape -> IO (Maybe (Ptr t))
downcast p = do
    e <- shapeType p
    return $ if e == (subtypeEnum (Proxy :: Proxy t)) 
                then Just (castPtr p)
                else Nothing

unsafeDowncast :: ShapeSubtype t => Ptr Shape -> IO (Ptr t)
unsafeDowncast p = do
    maybeT <- downcast p
    maybe (error "Incorrect shape subtype in cast") pure maybeT

instance ShapeSubtype Compound where
    subtypeEnum _ = ShapeEnum.Compound

instance ShapeSubtype CompSolid where
    subtypeEnum _ = ShapeEnum.CompSolid

instance ShapeSubtype Solid where
    subtypeEnum _ = ShapeEnum.Solid

instance ShapeSubtype Shell where
    subtypeEnum _ = ShapeEnum.Shell

instance ShapeSubtype Face where
    subtypeEnum _ = ShapeEnum.Face

instance ShapeSubtype Wire where
    subtypeEnum _ = ShapeEnum.Wire


instance ShapeSubtype Edge where
    subtypeEnum _ = ShapeEnum.Edge

instance ShapeSubtype Vertex where
    subtypeEnum _ = ShapeEnum.Vertex

