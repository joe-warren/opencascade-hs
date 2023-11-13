module Waterfall.Solids 
( Solid
, unitCube
, centeredCube
) where

import qualified OpenCascade.TopoDS.Solid as TDSolid
import qualified OpenCascade.BRepPrimAPI.MakeBox as MakeBox
import qualified OpenCascade.GP as GP
import qualified OpenCascade.GP.Pnt as GP.Pnt

import Foreign.Ptr
import Data.Acquire

newtype Solid = Solid { runSolid :: Acquire (Ptr TDSolid.Solid) }

unitCube :: Solid
unitCube = Solid $ do
    a <- GP.origin
    b <- GP.Pnt.new 1 1 1
    builder <- MakeBox.fromPnts a b
    MakeBox.solid builder


centeredCube :: Solid
centeredCube = Solid $ do
    a <- GP.Pnt.new (-1/2) (-1/2) (-1/2)
    b <- GP.Pnt.new (1/2) (1/2) (1/2)
    builder <- MakeBox.fromPnts a b
    MakeBox.solid builder