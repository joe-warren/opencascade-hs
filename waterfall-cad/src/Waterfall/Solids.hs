module Waterfall.Solids 
( Solid
, emptySolid
, unitCube
, centeredCube
, box
, unitSphere
, unitCylinder
, centeredCylinder
, unitCone
, torus
, tetrahedron
, octahedron
, dodecahedron
, icosahedron
, prism
, volume
, centerOfMass
, momentOfInertia
) where


import Waterfall.Internal.Solid (Solid (..), solidFromAcquire, acquireSolid, emptySolid)
import Waterfall.Internal.Finalizers (toAcquire, unsafeFromAcquire)
import Waterfall.TwoD.Internal.Shape (rawShape)
import Waterfall.Internal.ToOpenCascade (v3ToVertex)
import Waterfall.Internal.FromOpenCascade (gpPntToV3)
import Waterfall.Internal.Remesh (makeSolidFromShell)
import Waterfall.Transforms (translate, rotate)
import qualified Waterfall.TwoD.Shape as TwoD.Shape
import qualified OpenCascade.BRepBuilderAPI.MakeShape as MakeShape
import qualified OpenCascade.BRepBuilderAPI.MakeEdge as MakeEdge
import qualified OpenCascade.BRepBuilderAPI.MakeWire as MakeWire
import qualified OpenCascade.BRepBuilderAPI.MakeFace as MakeFace
import qualified OpenCascade.BRepBuilderAPI.Sewing as BRepBuilderAPI.Sewing
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Compound as TopoDS.Compound
import qualified OpenCascade.TopoDS.Builder as TopoDS.Builder
import qualified OpenCascade.BRepPrimAPI.MakeBox as MakeBox
import qualified OpenCascade.BRepPrimAPI.MakeSphere as MakeSphere
import qualified OpenCascade.BRepPrimAPI.MakeCylinder as MakeCylinder
import qualified OpenCascade.BRepPrimAPI.MakeCone as MakeCone
import qualified OpenCascade.BRepPrimAPI.MakeTorus as MakeTorus
import qualified OpenCascade.GProp.GProps as GProps
import qualified OpenCascade.BRepGProp as BRepGProp
import qualified OpenCascade.GP as GP
import Control.Lens ((^.), (&), (.~))
import Linear (V3 (..), V2 (..), unit, _x, _y, _z, _xy, _yz, _zx, (^*), (*^), unangle, zero)
import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.GP.Vec as GP.Vec
import qualified OpenCascade.GP.Dir as GP.Dir
import qualified OpenCascade.GP.Ax1 as GP.Ax1
import qualified OpenCascade.BRepPrimAPI.MakePrism as MakePrism
import qualified OpenCascade.Inheritance as Inheritance

import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<))
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire)

-- | A cube with side lengths of 1, one vertex on the origin, another on \( (1, 1, 1) \)
unitCube :: Solid
unitCube = solidFromAcquire $ do
    a <- GP.origin
    b <- GP.Pnt.new 1 1 1
    builder <- MakeBox.fromPnts a b
    Inheritance.upcast <$> MakeBox.solid builder

-- | A cube with side lengths of 1, centered on the origin
centeredCube :: Solid
centeredCube = solidFromAcquire $ do
    a <- GP.Pnt.new (-1/2) (-1/2) (-1/2)
    b <- GP.Pnt.new (1/2) (1/2) (1/2)
    builder <- MakeBox.fromPnts a b
    Inheritance.upcast <$> MakeBox.solid builder

-- | A cuboid, one vertex on the origin, another on a given point
box :: V3 Double -> Solid
box (V3 x y z) = solidFromAcquire $ do
    a <- GP.origin
    b <- GP.Pnt.new x y z
    builder <- MakeBox.fromPnts a b
    Inheritance.upcast <$> MakeBox.solid builder

    
-- | A sphere with radius of 1, centered on the origin
unitSphere :: Solid
unitSphere = solidFromAcquire $ Inheritance.upcast <$> MakeSphere.fromRadius 1

-- | A cylinder with radius 1, length 1,
-- one of it's circular faces centered on the origin,
-- the other centered on \( (0, 0, 1) \)
unitCylinder :: Solid
unitCylinder = solidFromAcquire $ Inheritance.upcast <$> MakeCylinder.fromRadiusAndHeight 1 1

-- | A cylinder with radius 1, length 1,
-- centered on the origin,
centeredCylinder :: Solid
centeredCylinder = translate (unit _z ^* (-0.5)) $ unitCylinder


-- | A Torus, with the axis of revolution about the Z axis
-- 
-- Warning, this will generate malformed geometry if asked to generate a Spindle Torus
-- (when the Major Radius is smaller than the Minor Radius)
torus ::
    Double -- ^ The Major Radius (Distance from center of torus to center of cube)
    -> Double -- ^ The Minor Radius (Distance from center of torus to center of )
    -> Solid
torus major minor = 
    solidFromAcquire
         $ MakeShape.shape 
         . Inheritance.upcast 
         =<< MakeTorus.fromRadii major minor

-- | A cone 
-- With a point at the origin 
-- and a circular face with Radius 1, centered on \( (0, 0, 1) \)
unitCone :: Solid
unitCone = solidFromAcquire $ Inheritance.upcast <$> MakeCone.fromTwoRadiiAndHeight 0 1 1

-- | Extruded a 2D face into a prism with a given length \(len\).
--
-- One of the prisms faces lies on the plane \(z = 0\),
-- the other on the plane \(z = len\).
prism :: Double -> TwoD.Shape.Shape -> Solid
prism len face = solidFromAcquire $ do
    p <- toAcquire . rawShape $ face
    v <- GP.Vec.new 0 0 len
    MakePrism.fromVec p v True True
    

faceFromVerts :: [V3 Double] -> Acquire (Ptr TopoDS.Face)
faceFromVerts pnts = do
    verts <- traverse v3ToVertex pnts
    edges <- traverse (uncurry MakeEdge.fromVertices) $ zip verts (drop 1 (cycle verts))
    wireBuilder <- MakeWire.new
    _ <- liftIO $ traverse (MakeWire.addEdge wireBuilder) edges
    wire <- MakeWire.wire wireBuilder
    -- passing True here forces the face to be a plane
    faceBuilder <- MakeFace.fromWire wire True
    MakeFace.face faceBuilder

solidFromFaces :: [Ptr TopoDS.Face] -> Acquire (Ptr TopoDS.Solid)
solidFromFaces faces = do
    sewing <- BRepBuilderAPI.Sewing.new 1e-6 True True True False
    compound <- TopoDS.Compound.new
    builder <- TopoDS.Builder.new
    liftIO $ TopoDS.Builder.makeCompound builder compound
    _ <- liftIO $ traverse (TopoDS.Builder.add builder (Inheritance.upcast compound) . Inheritance.upcast) faces
    liftIO $ BRepBuilderAPI.Sewing.load sewing (Inheritance.upcast compound)
    liftIO . BRepBuilderAPI.Sewing.perform $ sewing
    shape <- BRepBuilderAPI.Sewing.sewedShape sewing
    maybeShapeAsSolid <- makeSolidFromShell shape
    case maybeShapeAsSolid of 
        Just s -> return s
        Nothing -> error "Failed to construct solid from faces"

solidFromVerts :: [[V3 Double]] -> Solid
solidFromVerts = solidFromAcquire . fmap Inheritance.upcast . (solidFromFaces <=< traverse faceFromVerts)

-- | Regular Tetrahedron with unit side lengths
-- 
-- One vertex is in the Z direction
tetrahedron :: Solid
tetrahedron = 
    let r = ((1/sqrt 8) *^)
            . rotate (unit _z) (pi/4)
            . rotate (V3 1 1 0) (pi - unangle (V2 1 (sqrt 2)))
        v1 = V3 1 1 1
        v2 = V3 1 (-1) (-1)
        v3 = V3 (-1) 1 (-1)
        v4 = V3 (-1) (-1) 1
    in solidFromVerts . fmap (fmap r) $
        [ [v1, v2, v3] 
        , [v1 ,v2, v4]
        , [v2, v3, v4]
        , [v3, v1, v4]
        ]

-- | Regular Octahedron with unit side lengths
--
-- The vertices of the Octahedra lie on the X, Y and Z axes
octahedron :: Solid
octahedron = 
    let h = 1 / sqrt 2
        t = unit _z ^* h
        b = negate t
        c1 = h *^ unit _x
        c2 = h *^ unit _y
        c3 = negate c1
        c4 = negate c2
    in solidFromVerts
        [ [t, c1, c2]
        , [t, c2, c3]
        , [t, c3, c4]
        , [t, c4, c1]
        , [b, c2, c1]
        , [b, c3, c2]
        , [b, c4, c3]
        , [b, c1, c4]
        ]

-- | Regular Icosahedron with unit side lengths
icosahedron :: Solid
icosahedron = 
    let phi = (1 + sqrt 5) / 2
        signs = [-1, 1]
    in solidFromVerts $
        [ 
            let a = zero & l1 .~ (V2 (s1 * phi/2) 0.5)
                b = zero & l1 .~ (V2 (s1 * phi/2) (-0.5))
                c = zero & l2 .~ (V2 (s2 * phi/2) (0.5 * s1))
            in if s1 < 0 then [b, a, c]
                         else [a, b, c]
            | s1 <- signs
            , s2 <- signs
            , (l1, l2) <- [(_xy, _zx), (_zx, _yz), (_yz, _xy)]
        ] <> 
        [
            [ zero & _xy .~ (V2 (a * phi/2) (c * 0.5))
            , zero & _yz .~ (V2 (c * phi/2) (b * 0.5))
            , zero & _zx .~ (V2 (b * phi/2) (a * 0.5))
            ]
            | a <- signs
            , b <- signs
            , c <- signs
        ]

-- | Regular Dodecahedron with unit side lengths
dodecahedron :: Solid
dodecahedron = 
    let phi = (1 + sqrt 5)/2 
        plusMinusOne = [1, -1]
        scale = phi / 2 
        -- colours taken from https://commons.wikimedia.org/wiki/File:Dodecahedron_vertices.svg
        orange x y z = scale *^ V3 x y z
        green y z = scale *^ V3 0 (phi * y) (z / phi)
        blue x z = scale *^ V3 (x/phi) 0 (phi * z)
        pink x y = scale *^ V3 (phi * x) (y/phi) 0
    in solidFromVerts $
        [ \y z -> [ blue (-1) z, blue (1) z, orange (1) y z, green y z, orange (-1) y z]
        , \x y -> [ green y (-1), green y 1, orange x y 1, pink x y, orange x y (-1)]  
        , \x z -> [ pink x (-1), pink x 1, orange x 1 z, blue x z, orange x (-1) z] 
        ] <*> plusMinusOne <*> plusMinusOne

gPropQuery :: (Ptr GProps.GProps -> Acquire a) -> Solid -> a
gPropQuery f s = unsafeFromAcquire $ do
    solid <- acquireSolid s
    gProp <- GProps.new
    liftIO $ BRepGProp.volumeProperties solid gProp False False False
    f gProp

-- | Volume of the Solid
volume :: Solid -> Double
volume = gPropQuery (liftIO . GProps.mass)

-- | Center Of Mass of the Solid
centerOfMass :: Solid -> V3 Double 
centerOfMass = gPropQuery ((liftIO . gpPntToV3) <=< GProps.centreOfMass)

-- | Moment of Inertia of the Solid around a particular point and axis
momentOfInertia :: V3 Double -- ^ Point on the Axis of the Moment
    -> V3 Double -- ^ Direction of the Axis of the Moment 
    -> Solid
    -> Double 
momentOfInertia center axis = gPropQuery $ \gprop -> do
    pnt <- GP.Pnt.new (center ^. _x) (center ^. _y) (center ^. _z)
    dir <- GP.Dir.new (axis ^. _x) (axis ^. _y) (axis ^. _z)
    ax1 <- GP.Ax1.new pnt dir
    liftIO $ GProps.momentOfInertia gprop ax1
