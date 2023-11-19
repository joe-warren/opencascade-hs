module Waterfall.Solids 
( Solid
, unitCube
, centeredCube
, unitSphere
, unitCylinder
, prism
) where


import Waterfall.Internal.Solid(Solid(..))
import Waterfall.TwoD.Internal.Path (runPath)
import qualified Waterfall.TwoD.Path as TwoD.Path
import qualified OpenCascade.BRepPrimAPI.MakeBox as MakeBox
import qualified OpenCascade.BRepPrimAPI.MakeSphere as MakeSphere
import qualified OpenCascade.BRepPrimAPI.MakeCylinder as MakeCylinder
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.BRepPrimAPI.MakePrism as MakePrism
import qualified OpenCascade.Inheritance as Inheritance
import Linear.V3 (V3 (..))
import Data.Acquire

unitCube :: Solid
unitCube = Solid $ do
    a <- GP.origin
    b <- GP.Pnt.new 1 1 1
    builder <- MakeBox.fromPnts a b
    Inheritance.upcast <$> MakeBox.solid builder


centeredCube :: Solid
centeredCube = Solid $ do
    a <- GP.Pnt.new (-1/2) (-1/2) (-1/2)
    b <- GP.Pnt.new (1/2) (1/2) (1/2)
    builder <- MakeBox.fromPnts a b
    Inheritance.upcast <$> MakeBox.solid builder

unitSphere :: Solid
unitSphere = Solid $ Inheritance.upcast <$> MakeSphere.fromRadius 1

unitCylinder :: Solid
unitCylinder = Solid $ Inheritance.upcast <$> MakeCylinder.fromRadiusAndHeight 1 1

prism :: V3 Double -> TwoD.Path.Path -> Solid
prism (V3 x y z) path = Solid $ do
    p <- runPath path
    v <- GP.Vec.new x y z
    MakePrism.fromVec (Inheritance.upcast p) v True True



