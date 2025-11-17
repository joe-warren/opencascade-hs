{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE InstanceSigs #-}
module Waterfall.Internal.Solid 
( Solid (..)
, acquireSolid
, solidFromAcquire
, solidFromAcquireT
, union3D
, difference3D
, intersection3D
, unions3D
, intersections3D
, emptySolid
, complement
, debug
) where

import Data.Acquire
import Foreign.Ptr
import Algebra.Lattice
import Control.Monad.IO.Class (liftIO)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.BRepAlgoAPI.Fuse as Fuse
import qualified OpenCascade.BRepAlgoAPI.Cut as Cut
import qualified OpenCascade.BRepAlgoAPI.Common as Common
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.BOPAlgo.Operation as BOPAlgo.Operation
import qualified OpenCascade.BOPAlgo.BOP as BOPAlgo.BOP
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo.Builder
import OpenCascade.Inheritance (upcast)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire, unsafeFromAcquireT)
import qualified OpenCascade.BOPAlgo.Builder as BOPAlgo
import Data.Foldable (traverse_)

-- | The Boundary Representation of a solid object.
--
-- Alternatively, a region of 3d Space.
--
-- Under the hood, this is represented by an OpenCascade `TopoDS.Shape`.
-- The underlying shape should either be a Solid, or a CompSolid.
-- 
-- While you shouldn't need to know what this means to use the library,
-- please feel free to report a bug if you're able to construct a `Solid`
-- where this isnt' the case (without using internal functions).
newtype Solid = Solid { rawSolid :: Ptr TopoDS.Shape.Shape }

acquireSolid :: Solid -> Acquire (Ptr TopoDS.Shape.Shape)
acquireSolid (Solid ptr) = toAcquire ptr

solidFromAcquire :: Acquire (Ptr TopoDS.Shape.Shape) -> Solid
solidFromAcquire = Solid . unsafeFromAcquire

solidFromAcquireT :: Traversable t => Acquire (t (Ptr TopoDS.Shape.Shape)) -> t Solid
solidFromAcquireT = fmap Solid . unsafeFromAcquireT

-- | print debug information about a Solid when it's evaluated 
-- exposes the properties of the underlying OpenCacade.TopoDS.Shape
debug :: Solid -> String
debug (Solid ptr) = 
    let 
        fshow :: Show a => IO a -> IO String 
        fshow = fmap show
        actions = 
            [ ("type", fshow . TopoDS.Shape.shapeType)
            , ("closed", fshow . TopoDS.Shape.closed)
            , ("infinite", fshow . TopoDS.Shape.infinite)
            , ("orientable", fshow . TopoDS.Shape.orientable)
            , ("orientation", fshow . TopoDS.Shape.orientation)
            , ("null", fshow . TopoDS.Shape.isNull)
            , ("free", fshow . TopoDS.Shape.free)
            , ("locked", fshow . TopoDS.Shape.locked)
            , ("modified", fshow . TopoDS.Shape.modified)
            , ("checked",  fshow . TopoDS.Shape.checked)
            , ("convex", fshow . TopoDS.Shape.convex)
            , ("nbChildren", fshow . TopoDS.Shape.nbChildren)
            ]
    in unsafeFromAcquire $ do
        s <- toAcquire ptr
        liftIO $ (`foldMap` actions) $ \(actionName, value) -> 
                (return $ "\t" <> actionName <> "\t\t") <> value s <> (return "\n")

{--
-- TODO: this does not work, need to fix
everywhere :: Solid
everywhere = complement $ emptySolid
--}

-- | Invert a Solid, equivalent to `not` in boolean algebra.
--
-- The complement of a solid represents the solid with the same surface,
-- but where the opposite side of that surface is the \"inside\" of the solid.
--
-- Be warned that @complement emptySolid@ does not appear to work correctly.
complement :: Solid -> Solid
complement (Solid ptr) = Solid . unsafeFromAcquire $ TopoDS.Shape.complemented =<< toAcquire ptr

-- | An empty solid
--
-- Be warned that @complement emptySolid@ does not appear to work correctly.
emptySolid :: Solid 
emptySolid =  Solid . unsafeFromAcquire $ upcast <$> (MakeSolid.solid =<< MakeSolid.new)

-- defining the boolean CSG operators here, rather than in Waterfall.Booleans 
-- means that we can use them in typeclass instances without resorting to orphans

toBoolean :: (Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)) -> Solid -> Solid -> Solid
toBoolean f (Solid ptrA) (Solid ptrB) = Solid . unsafeFromAcquire $ do
    a <- toAcquire ptrA
    b <- toAcquire ptrB
    f a b

-- | Take the sum of two solids
--
-- The region occupied by either one of them.
union3D :: Solid -> Solid -> Solid
union3D = toBoolean Fuse.fuse


toBooleans :: BOPAlgo.Operation.Operation -> [Solid] -> Solid
toBooleans _ [] = emptySolid
toBooleans _ [x] = x
toBooleans op (h:solids) = Solid . unsafeFromAcquire $ do
    firstPtr <- toAcquire . rawSolid $ h
    ptrs <- traverse (toAcquire . rawSolid) solids
    bop <- BOPAlgo.BOP.new
    let builder = upcast bop
    liftIO $ do
        BOPAlgo.BOP.setOperation bop op
        BOPAlgo.Builder.addArgument builder firstPtr
        traverse_ (BOPAlgo.BOP.addTool bop) ptrs
        BOPAlgo.setRunParallel builder True
        BOPAlgo.Builder.perform builder
    BOPAlgo.Builder.shape builder

-- | Take the sum of a list of solids 
-- 
-- May be more performant than chaining multiple applications of `union3D`
unions3D :: [Solid] -> Solid
unions3D = toBooleans BOPAlgo.Operation.Fuse

-- | Take the difference of two solids
-- 
-- The region occupied by the first, but not the second.
difference3D :: Solid -> Solid -> Solid
difference3D = toBoolean Cut.cut

-- | Take the intersection of two solids 
--
-- The region occupied by both of them.
intersection3D :: Solid -> Solid -> Solid
intersection3D = toBoolean Common.common


-- | Take the intersection of a list of solids 
-- 
-- May be more performant than chaining multiple applications of `intersection3D`
intersections3D :: [Solid] -> Solid
intersections3D = toBooleans BOPAlgo.Operation.Common

-- | While `Solid` could form a Semigroup via either `union3D` or `intersection3D`.
-- the default Semigroup is from `union3D`.
--
-- The Semigroup from `intersection3D` can be obtained using `Meet` from the lattices package.
instance Semigroup Solid where
    (<>) :: Solid -> Solid -> Solid
    (<>) = union3D

instance Monoid Solid where
    mempty = emptySolid
    mconcat = unions3D

instance Lattice Solid where 
    (/\) = intersection3D
    (\/) = union3D

instance BoundedJoinSemiLattice Solid where
    bottom = emptySolid

{--
-- TODO: because everywhere doesn't work correctly
-- using the BoundedMeetSemiLattice instance
-- and by extension, the Heyting instance
-- is liable to produce invalid shapes
instance BoundedMeetSemiLattice Solid where
    top = everywhere

-- every boolean algebra is a Heyting algebra with
--  a → b defined as ¬a ∨ b
instance Heyting Solid where
    neg = complement
    a ==> b = neg a \/ b
--}