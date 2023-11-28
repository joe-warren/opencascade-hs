{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE InstanceSigs #-}
module Waterfall.Internal.Solid 
( Solid (..)
, union
, difference
, intersection
, nowhere
, complement
, debug
) where

import Data.Acquire
import Foreign.Ptr
import Algebra.Lattice
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.BRepAlgoAPI.Fuse as Fuse
import qualified OpenCascade.BRepAlgoAPI.Cut as Cut
import qualified OpenCascade.BRepAlgoAPI.Common as Common
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import OpenCascade.Inheritance (upcast)

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
newtype Solid = Solid { runSolid :: Acquire (Ptr TopoDS.Shape.Shape) }

-- | print debug information about a Solid when it's evaluated 
-- exposes the properties of the underlying OpenCacade.TopoDS.Shape
debug :: String -> Solid -> Solid
debug name (Solid runTheSolid) = 
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
    in Solid $ do
    s <- runTheSolid
    liftIO $ do 
        putStrLn name
        forM_ actions $ \(actionName, value) -> do
            putStrLn ("\t" <> actionName)
            putStrLn . ("\t\t" <>) =<< value s
    return s

{--
-- TODO: this does not work, need to fix
everywhere :: Solid
everywhere = complement $ nowhere
--}

-- | Invert a Solid, equivalent to `not` in boolean algebra.
--
-- The complement of a solid represents the solid with the same surface,
-- but where the opposite side of that surface is the \"inside\" of the solid.
--
-- Be warned that @complement nowhere@ does not appear to work correctly.
complement :: Solid -> Solid
complement (Solid run) = Solid $ TopoDS.Shape.complemented =<< run

-- | An empty solid
--
-- Be warned that @complement nowhere@ does not appear to work correctly.
nowhere :: Solid 
nowhere =  Solid $ upcast <$> (MakeSolid.solid =<< MakeSolid.new)

-- defining the boolean CSG operators here, rather than in Waterfall.Booleans 
-- means that we can use them in typeclass instances without resorting to orphans

toBoolean :: (Ptr TopoDS.Shape -> Ptr TopoDS.Shape -> Acquire (Ptr TopoDS.Shape)) -> Solid -> Solid -> Solid
toBoolean f (Solid runA) (Solid runB) = Solid $ do
    a <- runA
    b <- runB
    f a b

-- | Take the sum of two solids
--
-- The region occupied by either one of them.
union :: Solid -> Solid -> Solid
union = toBoolean Fuse.fuse

-- | Take the difference of two solids
-- 
-- The region occupied by the first, but not the second.
difference :: Solid -> Solid -> Solid
difference = toBoolean Cut.cut

-- | Take the intersection of two solids 
--
-- The region occupied by both of them.
intersection :: Solid -> Solid -> Solid
intersection = toBoolean Common.common

-- | While `Solid` could form a Semigroup via either `union` or `intersection`.
-- the default Semigroup is from `union`.
--
-- The Semigroup from `intersection` can be obtained using `Meet` from the lattices package.
instance Semigroup Solid where
    (<>) :: Solid -> Solid -> Solid
    (<>) = union

instance Monoid Solid where
    mempty = nowhere

instance Lattice Solid where 
    (/\) = intersection
    (\/) = union

instance BoundedJoinSemiLattice Solid where
    bottom = nowhere
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