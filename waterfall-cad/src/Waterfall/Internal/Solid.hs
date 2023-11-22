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
import Algebra.Heyting
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.BRepAlgoAPI.Fuse as Fuse
import qualified OpenCascade.BRepAlgoAPI.Cut as Cut
import qualified OpenCascade.BRepAlgoAPI.Common as Common
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.BRepPrimAPI.MakeBox as MakeBox
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.TopoDS.Solid as TopoDS.Solid
import qualified OpenCascade.TopoDS.CompSolid as TopoDS.CompSolid
import OpenCascade.Inheritance (upcast)

newtype Solid = Solid { runSolid :: Acquire (Ptr TopoDS.Shape.Shape) }

debug :: String -> Solid -> Solid
debug name (Solid runSolid) = 
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
    s <- runSolid
    liftIO $ do 
        putStrLn name
        forM_ actions $ \(name, value) -> do
            putStrLn ("\t" <> name)
            putStrLn =<< ("\t\t" <>) <$> value s
    return s

{--
-- TODO: this does not work, need to fix
everywhere :: Solid
everywhere = complement $ nowhere
--}

complement :: Solid -> Solid
complement (Solid runSolid) = Solid $ TopoDS.Shape.complemented =<< runSolid 

nowhere :: Solid 
nowhere =  Solid $ upcast <$> (MakeSolid.solid =<< MakeSolid.new)

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