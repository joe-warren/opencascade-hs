module Waterfall.Solids 
( Solid
, unitCube
, centeredCube
, unitSphere
, unitCylinder
) where


import Waterfall.Internal.Solid(Solid(..))
import qualified OpenCascade.BRepPrimAPI.MakeBox as MakeBox
import qualified OpenCascade.BRepPrimAPI.MakeSphere as MakeSphere
import qualified OpenCascade.BRepPrimAPI.MakeCylinder as MakeCylinder
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.Inheritance as Inheritance
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