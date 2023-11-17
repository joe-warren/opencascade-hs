module Waterfall.Internal.Solid 
( Solid (..)
, union
, difference
, intersection
) where

import Data.Acquire
import Foreign.Ptr
import Algebra.Lattice
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape

import qualified OpenCascade.BRepAlgoAPI.Fuse as Fuse
import qualified OpenCascade.BRepAlgoAPI.Cut as Cut
import qualified OpenCascade.BRepAlgoAPI.Common as Common
import qualified OpenCascade.TopoDS as TopoDS

newtype Solid = Solid { runSolid :: Acquire (Ptr TopoDS.Shape.Shape) }

-- defining the boolean CSG operators here, rather than in Waterfall.Booleans 
-- means that we can use them in typeclass instances without resorting to orphans

toBoolean :: (Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)) -> Solid -> Solid -> Solid
toBoolean f (Solid runA) (Solid runB) = Solid $ do
    a <- runA
    b <- runB
    f a b

union :: Solid -> Solid -> Solid
union = toBoolean Fuse.fuse

difference :: Solid -> Solid -> Solid
difference = toBoolean Cut.cut

intersection :: Solid -> Solid -> Solid
intersection = toBoolean Common.common

instance Semigroup Solid where
    (<>) = union

instance Lattice Solid where 
    (/\) = intersection
    (\/) = union
