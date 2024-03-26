{-| 
Module: Waterfall.Internal.Remesh

This code exists because the opencascade GLTF loading code generates "weird" BReps

The FreeCAD sourcecode describes this as follows:

> The glTF reader creates a compound of faces that only contains the triangulation
> but not the underlying surfaces. This leads to faces without boundaries.
> The triangulation is used to create a valid shape.

The practical result of this, seems to be that directly using an `OpenCascade.TopoDS.Shape` 
loaded using `OpenCascade.RWGltf.CafReader` in most operations will lead to segmentation faults.

However, we can safely access the Triangulation of the Shape, construct Polygons from this
and then use BReps derived from these Polygons.

In this way, the `remesh` function produces a new Boundary Represenation from the Mesh of an `OpenCascade.TopoDS.Shape`

-}
module Waterfall.Internal.Remesh 
( remesh 
, checkNonNull
) where

import qualified OpenCascade.GP.Pnt as GP.Pnt
import qualified OpenCascade.TopoDS as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.TopoDS.Compound as TopoDS.Compound
import qualified OpenCascade.TopExp.Explorer as TopExp.Explorer
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.TopAbs.Orientation as TopAbs.Orientation
import qualified OpenCascade.BRepBuilderAPI.Sewing as BRepBuilderAPI.Sewing
import qualified OpenCascade.BRepBuilderAPI.MakePolygon as BRepBuilderAPI.MakePolygon
import qualified OpenCascade.BRepBuilderAPI.MakeFace as BRepBuilderAPI.MakeFace
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as BRepBuilderAPI.MakeSolid
import qualified OpenCascade.BRepBuilderAPI.MakeShape as BRepBuilderAPI.MakeShape
import qualified OpenCascade.BRep.Tool as BRep.Tool
import qualified OpenCascade.BRepLib as BRepLib
import qualified OpenCascade.TopoDS.Builder as TopoDS.Builder
import qualified OpenCascade.BRepMesh.IncrementalMesh as BRepMesh.IncrementalMesh
import qualified OpenCascade.Poly.Triangulation as Poly.Triangulation
import qualified OpenCascade.Poly.Triangle as Poly.Triangle
import qualified OpenCascade.TopLoc.Location as TopLoc.Location
import OpenCascade.Inheritance (upcast, unsafeDowncast)
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when, unless, forM_, (<=<))


checkNonNull:: MonadIO m => Ptr TopoDS.Shape -> m (Maybe (Ptr TopoDS.Shape))
checkNonNull shape = do
    isNull <- liftIO . TopoDS.Shape.isNull $ shape
    return $ if isNull 
        then Nothing
        else Just shape

remesh :: Ptr TopoDS.Shape -> Acquire (Maybe (Ptr TopoDS.Shape))
remesh s = do

    let linDeflection = 0.01
    mesh <- BRepMesh.IncrementalMesh.fromShapeAndLinDeflection s linDeflection
    liftIO $ BRepMesh.IncrementalMesh.perform mesh

    builder <- TopoDS.Builder.new
    compound <- TopoDS.Compound.new
    liftIO $ TopoDS.Builder.makeCompound builder compound
    sewing <- BRepBuilderAPI.Sewing.new 1e-6 True True True False
    explorer <- TopExp.Explorer.new s ShapeEnum.Face
    
    let actionForEachFace :: Acquire ()
        actionForEachFace = do
            faceAsShape <- liftIO $ TopExp.Explorer.value explorer
            faceAsFace <- liftIO . unsafeDowncast $ faceAsShape
            loc <- TopLoc.Location.new
            orientation <- liftIO $ TopoDS.Shape.orientation faceAsShape
            trsf <- TopLoc.Location.toGPTrsf loc
            triangulation <- BRep.Tool.triangulation faceAsFace loc
            triCount <- liftIO $ Poly.Triangulation.nbTriangles triangulation
            forM_ [1..triCount] $ \i -> do
                triangle <- Poly.Triangulation.triangle triangulation i
                let p = (`GP.Pnt.transformed` trsf) <=< Poly.Triangulation.node triangulation <=< liftIO . Poly.Triangle.value triangle 
                p1 <- p 1
                p2 <- p 2
                p3 <- p 3
                let pointsEqual a b = liftIO $ GP.Pnt.isEqual a b 0
                p12Coincident <- pointsEqual p1 p2 
                p13Coincident <- pointsEqual p1 p3
                p23Coincident <- pointsEqual p2 p3
                let anyPointsCoincident = p12Coincident || p13Coincident || p23Coincident
                unless anyPointsCoincident $ do
                    let makePolygon p1' p2' p3' = BRepBuilderAPI.MakePolygon.from3Pnts p1' p2' p3' True
                    polygon <- if orientation == TopAbs.Orientation.Reversed 
                        then makePolygon p1 p3 p2
                        else makePolygon p1 p2 p3
                    polygonIsNull <- liftIO $ TopoDS.Shape.isNull (upcast polygon)
                    unless polygonIsNull $ do
                        makeFace <- BRepBuilderAPI.MakeFace.fromWire polygon False
                        newFace <- BRepBuilderAPI.MakeShape.shape (upcast makeFace)
                        faceIsNull <- liftIO $ TopoDS.Shape.isNull newFace
                        unless faceIsNull $ liftIO $ TopoDS.Builder.add builder (upcast compound) newFace 

    let go = do
            isMore <- liftIO $ TopExp.Explorer.more explorer
            when isMore $ do 
                actionForEachFace
                liftIO $ TopExp.Explorer.next explorer
                go
    go
    liftIO $ BRepBuilderAPI.Sewing.load sewing (upcast compound)
    liftIO . BRepBuilderAPI.Sewing.perform $ sewing
    liftIO $ print=<< BRepBuilderAPI.Sewing.nbFreeEdges sewing
    liftIO $ print=<< BRepBuilderAPI.Sewing.nbContigousEdges sewing
    liftIO $ print=<< BRepBuilderAPI.Sewing.nbMultipleEdges sewing
    shape <- BRepBuilderAPI.Sewing.sewedShape sewing
    makeSolid <- BRepBuilderAPI.MakeSolid.new 
    shapeAsShell <- liftIO $ unsafeDowncast shape
    liftIO $ BRepBuilderAPI.MakeSolid.add makeSolid shapeAsShell
    shapeAsSolid <- BRepBuilderAPI.MakeSolid.solid makeSolid
    maybeNotNull <- checkNonNull (upcast shapeAsSolid)
    case maybeNotNull of
        Nothing -> return Nothing
        Just _ -> do 
            orientable <- liftIO $ BRepLib.orientClosedSolid (shapeAsSolid)
            if orientable 
                then return . Just . upcast $ shapeAsSolid
                else return Nothing
            

    
