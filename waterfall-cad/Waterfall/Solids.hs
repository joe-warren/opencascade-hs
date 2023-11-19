module Waterfall.Solids 
( Solid
, unitCube
, centeredCube
, unitSphere
, unitCylinder
, prism
) where


import Waterfall.Internal.Solid(Solid(..))
import Waterfall.TwoD.Internal.Shape (runShape)
import qualified Waterfall.TwoD.Shape as TwoD.Shape
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

prism :: Double -> TwoD.Shape.Shape -> Solid
prism length face = Solid $ do
    p <- runShape face
    v <- GP.Vec.new 0 0 length
    MakePrism.fromVec p v True True



