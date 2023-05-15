{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}
module Lib
    ( someFunc
    ) where
import Foreign.C
import Foreign.Ptr
import Control.Monad (forM_, filterM)
import Data.Foldable (traverse_, maximumBy)
import Data.Function (on)

someFunc :: IO ()
someFunc = print "hello world"


{--
data TopoDS_Shape
data TopoDS_Edge
data TopoDS_Wire
data TopoDS_Face
data TopoDS_Compound
data GP_Pnt
data GP_Ax1
data GP_Ax2
data GP_Ax2d
data GP_Pnt2d
data GP_Dir
data GP_Dir2d
data GP_Vec
data GP_Trsf
data BRep_Builder
data BRepBuilderAPI_Transform
data BRepBuilderAPI_MakeWire
data BRepFilletAPI_MakeFillet
data TopAbs_ShapeEnum
data TopExp_Explorer
data BRepPrimAPI_MakeCylinder
data TopTools_ListOfShape
data BRepOffsetAPI_MakeThickSolid
data Geom2d_Ellipse
data BRepOffsetAPI_ThruSections
data Handle_Geom_TrimmedCurve
data Handle_Geom_Surface
data Handle_Geom_CylindricalSurface
data Handle_Geom_Plane
data Handle_Geom2d_TrimmedCurve

foreign import capi unsafe "bottle.h hs_new_GP_Pnt" new_GP_Pnt :: CDouble -> CDouble -> CDouble -> IO (Ptr GP_Pnt)

foreign import capi unsafe "bottle.h hs_new_GP_Pnt2d" new_GP_Pnt2d :: CDouble -> CDouble -> IO (Ptr GP_Pnt2d)

foreign import capi unsafe "bottle.h hs_new_GP_Dir2d" new_GP_Dir2d :: CDouble -> CDouble -> IO (Ptr GP_Dir2d)

foreign import capi unsafe "bottle.h hs_new_GP_Vec" new_GP_Vec :: CDouble -> CDouble -> CDouble -> IO (Ptr GP_Vec)

foreign import capi unsafe "bottle.h hs_GCMakeArcOfCircle" gcMakeArcOfCircle :: Ptr GP_Pnt -> Ptr GP_Pnt -> Ptr GP_Pnt -> IO (Ptr Handle_Geom_TrimmedCurve)

foreign import capi unsafe "bottle.h hs_GC_MakeSegment" gcMakeSegment :: Ptr GP_Pnt -> Ptr GP_Pnt -> IO (Ptr Handle_Geom_TrimmedCurve)

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_MakeEdgeTrimmedCurve" brepBuilderAPI_MakeEdgeTrimmedCurve :: Ptr Handle_Geom_TrimmedCurve -> IO (Ptr TopoDS_Edge)

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_MakeWire_2Edges" brepBuilderAPI_MakeWire_2Edges :: Ptr TopoDS_Edge -> Ptr TopoDS_Edge -> IO (Ptr TopoDS_Wire)

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_MakeWire_3Edges" brepBuilderAPI_MakeWire_3Edges :: Ptr TopoDS_Edge -> Ptr TopoDS_Edge -> Ptr TopoDS_Edge -> IO (Ptr TopoDS_Wire)

foreign import capi unsafe "bottle.h hs_gp_Ax1_OX" gp_Ax1_OX :: IO (Ptr GP_Ax1)

foreign import capi unsafe "bottle.h hs_gp_Ax1_OY" gp_Ax1_OY :: IO (Ptr GP_Ax1)

foreign import capi unsafe "bottle.h hs_gp_Ax1_OZ" gp_Ax1_OZ :: IO (Ptr GP_Ax1)
 
foreign import capi unsafe "bottle.h hs_gp_Dir_DX" gp_Dir_DX :: IO (Ptr GP_Dir)

foreign import capi unsafe "bottle.h hs_gp_Dir_DY" gp_Dir_DY :: IO (Ptr GP_Dir)

foreign import capi unsafe "bottle.h hs_gp_Dir_DZ" gp_Dir_DZ :: IO (Ptr GP_Dir)

foreign import capi unsafe "bottle.h hs_new_Trsf" new_GP_Trsf :: IO (Ptr GP_Trsf)

foreign import capi unsafe "bottle.h hs_Trsf_SetMirror" gp_Trsf_SetMirror :: Ptr GP_Trsf -> Ptr GP_Ax1 -> IO ()

foreign import capi unsafe "bottle.h hs_new_BRepBuilderAPI_Transform" new_BRepBuilderAPI_Transform :: Ptr TopoDS_Wire -> Ptr GP_Trsf -> IO (Ptr BRepBuilderAPI_Transform)

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_Transform_Shape" brepBuilderAPI_Transform_Shape :: Ptr BRepBuilderAPI_Transform -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_BRepBuilderAPI_MakeWire" new_BRepBuilderAPI_MakeWire :: IO (Ptr BRepBuilderAPI_MakeWire)

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_MakeWire_AddWire" brepBuilderAPI_MakeWire_AddWire :: Ptr BRepBuilderAPI_MakeWire -> Ptr TopoDS_Wire -> IO ()

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_MakeWire_Wire" brepBuilderAPI_MakeWire_Wire :: Ptr BRepBuilderAPI_MakeWire -> IO (Ptr TopoDS_Wire)

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_MakeFace_Wire" brepBuilderAPI_MakeFace_Wire :: Ptr TopoDS_Wire -> IO (Ptr TopoDS_Face)

foreign import capi unsafe "bottle.h hs_BRepPrimAPI_MakePrism" brepPrimAPI_MakePrism :: Ptr TopoDS_Face -> Ptr GP_Vec -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_BRepFilletAPI_MakeFillet" new_BRepFilletAPI_MakeFillet :: Ptr TopoDS_Shape -> IO (Ptr BRepFilletAPI_MakeFillet)

foreign import capi unsafe "bottle.h hs_BRepFilletAPI_MakeFillet_Add" brepFilletAPI_MakeFillet_Add :: Ptr BRepFilletAPI_MakeFillet -> CDouble -> Ptr TopoDS_Edge -> IO ()

foreign import capi unsafe "bottle.h hs_BRepFilletAPI_MakeFillet_Shape" brepFilletAPI_MakeFillet_Shape :: Ptr BRepFilletAPI_MakeFillet -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_TopAbs_EDGE" new_TopAbs_EDGE :: IO (Ptr TopAbs_ShapeEnum)

foreign import capi unsafe "bottle.h hs_new_TopAbs_FACE" new_TopAbs_FACE :: IO (Ptr TopAbs_ShapeEnum)

foreign import capi unsafe "bottle.h hs_new_TopExp_Explorer" new_TopExp_Explorer :: Ptr TopoDS_Shape -> Ptr TopAbs_ShapeEnum -> IO (Ptr TopExp_Explorer)

foreign import capi unsafe "bottle.h hs_TopExp_Explorer_More" topExp_Explorer_More :: Ptr TopExp_Explorer -> IO CBool

foreign import capi unsafe "bottle.h hs_TopExp_Explorer_Current" topExp_Explorer_Current :: Ptr TopExp_Explorer -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_TopExp_Explorer_Next" topExp_Explorer_Next :: Ptr TopExp_Explorer -> IO ()

foreign import capi unsafe "bottle.h hs_BRep_Tool_Surface" brep_Tool_Surface :: Ptr TopoDS_Face -> IO (Ptr Handle_Geom_Surface)

foreign import capi unsafe "bottle.h hs_Geom_Surface_isPlane" geom_Surface_isPlane :: Ptr Handle_Geom_Surface -> IO CBool

foreign import capi unsafe "bottle.h hs_Geom_Plane_Location" geom_Plane_Location :: Ptr Handle_Geom_Plane -> IO (Ptr GP_Pnt)

foreign import capi unsafe "bottle.h hs_gp_Pnt_X" gp_Pnt_X :: Ptr GP_Pnt -> IO CDouble

foreign import capi unsafe "bottle.h hs_gp_Pnt_Y" gp_Pnt_Y :: Ptr GP_Pnt -> IO CDouble

foreign import capi unsafe "bottle.h hs_gp_Pnt_Z" gp_Pnt_Z :: Ptr GP_Pnt -> IO CDouble

foreign import capi unsafe "bottle.h hs_new_gp_Ax2" new_GP_Ax2 :: Ptr GP_Pnt -> Ptr GP_Dir -> IO (Ptr GP_Ax2)

foreign import capi unsafe "bottle.h hs_new_gp_Ax2d" new_GP_Ax2d :: Ptr GP_Pnt2d -> Ptr GP_Dir2d -> IO (Ptr GP_Ax2d)

foreign import capi unsafe "bottle.h hs_new_BRepPrimAPI_MakeCylinder" new_BRepPrimAPI_MakeCylinder :: Ptr GP_Ax2 -> CDouble -> CDouble -> IO (Ptr BRepPrimAPI_MakeCylinder)

foreign import capi unsafe "bottle.h hs_BRepPrimAPI_MakeCylinder_Shape" brepPrimAPI_MakeCylinder_Shape :: Ptr BRepPrimAPI_MakeCylinder -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_BRepAlgoAPI_Fuse" brepAlgoAPI_Fuse :: Ptr TopoDS_Shape -> Ptr TopoDS_Shape -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_TopTools_ListOfShape" new_TopTools_ListOfShape :: IO (Ptr TopTools_ListOfShape)

foreign import capi unsafe "bottle.h hs_TopTools_ListOfShape_Append" topTools_ListOfShape_Append :: Ptr TopTools_ListOfShape -> Ptr TopoDS_Shape -> IO ()

foreign import capi unsafe "bottle.h hs_new_BRepOffsetAPI_MakeThickSolid" new_BRepOffsetAPI_MakeThickSolid :: IO (Ptr BRepOffsetAPI_MakeThickSolid)

foreign import capi unsafe "bottle.h hs_BRepOffsetAPI_MakeThickSolid_MakeThickSolidByJoin" brepOffsetAPI_MakeThickSolid_MakeThickSolidByJoin :: Ptr BRepOffsetAPI_MakeThickSolid -> Ptr TopoDS_Shape -> Ptr TopTools_ListOfShape -> CDouble -> CDouble -> IO ()

foreign import capi unsafe "bottle.h hs_BRepOffsetAPI_MakeThickSolid_Shape" brepOffsetAPI_MakeThickSolid_Shape :: Ptr BRepOffsetAPI_MakeThickSolid -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_GeomCylindricalSurface" new_Geom_CylindricalSurface :: Ptr GP_Ax2 -> CDouble -> IO (Ptr Handle_Geom_CylindricalSurface)

foreign import capi unsafe "bottle.h hs_new_Geom2d_Ellipse" new_Geom2d_Ellipse :: Ptr GP_Ax2d -> CDouble -> CDouble -> IO (Ptr Geom2d_Ellipse)

foreign import capi unsafe "bottle.h hs_new_Geom2d_Trimmed_Curve_fromEllipse" new_Geom2d_TrimmedCurve_fromEllipse :: Ptr Geom2d_Ellipse -> CDouble -> CDouble -> IO (Ptr Handle_Geom2d_TrimmedCurve)

foreign import capi unsafe "bottle.h hs_Geom2d_Ellipse_Value" geom2d_Ellipse_Value :: Ptr Geom2d_Ellipse -> CDouble -> IO (Ptr GP_Pnt2d)

foreign import capi unsafe "bottle.h hs_GCE2d_MakeSegment" gce2d_MakeSegment :: Ptr GP_Pnt2d -> Ptr GP_Pnt2d -> IO (Ptr Handle_Geom2d_TrimmedCurve)

foreign import capi unsafe "bottle.h hs_BRepBuilderAPI_MakeEdge_fromArcAndCylinder" brepBuilderAPI_MakeEdge_fromArcAndCylinder :: Ptr Handle_Geom2d_TrimmedCurve -> Ptr Handle_Geom_CylindricalSurface -> IO (Ptr TopoDS_Edge)

foreign import capi unsafe "bottle.h hs_BRepLib_BuildCurves3d" brepLib_BuildCurves3d :: Ptr TopoDS_Wire -> IO ()

foreign import capi unsafe "bottle.h hs_new_BRepOffsetAPI_ThruSections" new_BRepOffsetAPI_ThruSections :: CBool -> IO (Ptr BRepOffsetAPI_ThruSections)

foreign import capi unsafe "bottle.h hs_BRepOffsetAPI_ThruSections_AddWire" brepOffsetAPI_ThruSections_AddWire :: Ptr BRepOffsetAPI_ThruSections -> Ptr TopoDS_Wire -> IO ()

foreign import capi unsafe "bottle.h hs_BRepOffsetAPI_ThruSections_CheckCompatibility" brepOffsetAPI_ThruSections_CheckCompatibility :: Ptr BRepOffsetAPI_ThruSections -> CBool -> IO ()

foreign import capi unsafe "bottle.h hs_BRepOffsetAPI_ThruSections_Shape" brepOffsetAPI_ThruSections_Shape :: Ptr BRepOffsetAPI_ThruSections -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_TopoDS_Compound" new_TopoDS_Compound :: IO (Ptr TopoDS_Compound)

foreign import capi unsafe "bottle.h hs_new_BRepBuilder" new_BRepBuilder :: IO (Ptr BRep_Builder)

foreign import capi unsafe "bottle.h hs_BRep_Builder_MakeCompound" brep_Builder_MakeCompound :: Ptr BRep_Builder -> Ptr TopoDS_Compound -> IO ()

foreign import capi unsafe "bottle.h hs_BRep_Builder_Add" brep_Builder_Add :: Ptr BRep_Builder -> Ptr TopoDS_Compound -> Ptr TopoDS_Shape -> IO ()

foreign import capi unsafe "bottle.h SaveShapeSTL" saveShapeSTL :: CDouble -> Ptr TopoDS_Shape -> CString -> IO CInt
foreign import capi unsafe "bottle.h MakeBottle" makeBottleCPP :: CDouble -> CDouble -> CDouble -> IO (Ptr TopoDS_Shape)


shapeFeatures :: Ptr TopAbs_ShapeEnum -> Ptr TopoDS_Shape -> IO [Ptr TopoDS_Shape]
shapeFeatures theType shape = do
    explorer <- new_TopExp_Explorer shape theType
    let go = do
                more <- topExp_Explorer_More explorer
                if more /= 0
                    then do
                        current <- topExp_Explorer_Current explorer 
                        topExp_Explorer_Next explorer
                        (current :) <$> go
                    else return []
    go

shapeEdges :: Ptr TopoDS_Shape -> IO [Ptr TopoDS_Edge]
shapeEdges shape = do
    edgeType <- new_TopAbs_EDGE
    fmap (fmap castPtr) $ shapeFeatures edgeType shape

shapeFaces :: Ptr TopoDS_Shape -> IO [Ptr TopoDS_Face]
shapeFaces shape = do
    faceType <- new_TopAbs_FACE
    fmap (fmap castPtr) $ shapeFeatures faceType shape

filletShape ::  (Ptr TopoDS_Edge -> IO (Maybe CDouble)) -> Ptr TopoDS_Shape -> IO (Ptr TopoDS_Shape)
filletShape f theShape = do
    fillet <- new_BRepFilletAPI_MakeFillet theShape
    edges <- shapeEdges theShape
    forM_ edges $ 
        \e -> do
                r <- f e
                case r of
                    Nothing -> pure ()
                    Just t -> brepFilletAPI_MakeFillet_Add fillet t e
    brepFilletAPI_MakeFillet_Shape fillet

listOfShape :: [Ptr TopoDS_Shape] -> IO (Ptr TopTools_ListOfShape)
listOfShape shapes = do
    l <- new_TopTools_ListOfShape
    traverse_ (topTools_ListOfShape_Append l) shapes
    pure l

thickenSolid :: ([Ptr TopoDS_Face] -> IO [Ptr TopoDS_Face]) -> CDouble -> CDouble -> Ptr TopoDS_Shape -> IO (Ptr TopoDS_Shape)
thickenSolid f thickness tolerance shape = do
    allFaces <- shapeFaces shape
    relevantFaces <- f allFaces 
    l <- listOfShape (castPtr <$> relevantFaces)
    offset <- new_BRepOffsetAPI_MakeThickSolid
    brepOffsetAPI_MakeThickSolid_MakeThickSolidByJoin offset shape l thickness tolerance
    brepOffsetAPI_MakeThickSolid_Shape offset

castSurfaceToPlane :: Ptr Handle_Geom_Surface -> IO (Maybe (Ptr Handle_Geom_Plane))
castSurfaceToPlane surf = do
    isPlane <- geom_Surface_isPlane surf
    if isPlane /= 0
        then return . Just . castPtr $ surf
        else return Nothing

faceZ :: Ptr TopoDS_Face -> IO (Maybe CDouble)
faceZ face = do
    surface <- brep_Tool_Surface face
    maybePlane <- castSurfaceToPlane surface
    case maybePlane of
        Nothing -> pure Nothing
        Just plane -> do
            loc <- geom_Plane_Location plane
            Just <$> gp_Pnt_Z loc

findNeck :: [Ptr TopoDS_Face] -> IO [Ptr TopoDS_Face]
findNeck faces = do
    heights <- traverse faceZ faces
    pure . pure . snd . maximumBy (compare `on` fst) $ zip heights faces

makeBottleHS :: CDouble -> CDouble -> CDouble -> IO (Ptr TopoDS_Shape)
makeBottleHS theWidth theHeight theThickness = do
    
    aPnt1 <- new_GP_Pnt (-theWidth/2) 0 0
    aPnt2 <- new_GP_Pnt (-theWidth/2) (-theThickness/4) 0
    aPnt3 <- new_GP_Pnt 0 (-theThickness/2) 0
    aPnt4 <- new_GP_Pnt (theWidth/2) (-theThickness/4) 0
    aPnt5 <- new_GP_Pnt (theWidth/2) 0 0

    anArcOfCircle <- gcMakeArcOfCircle aPnt2 aPnt3 aPnt4
    aSegment1 <- gcMakeSegment aPnt1 aPnt2
    aSegment2 <- gcMakeSegment aPnt4 aPnt5

    anEdge1 <- brepBuilderAPI_MakeEdgeTrimmedCurve aSegment1
    anEdge2 <- brepBuilderAPI_MakeEdgeTrimmedCurve anArcOfCircle
    anEdge3 <- brepBuilderAPI_MakeEdgeTrimmedCurve aSegment2
    aWire <- brepBuilderAPI_MakeWire_3Edges anEdge1 anEdge2 anEdge3 
    
    ax1X <- gp_Ax1_OX
    aTrsf <- new_GP_Trsf
    gp_Trsf_SetMirror aTrsf ax1X
    aBRepTrsf <- new_BRepBuilderAPI_Transform aWire aTrsf
    aMirroredShape <- brepBuilderAPI_Transform_Shape aBRepTrsf
    let aMirroredWire = castPtr aMirroredShape
    mkWire <- new_BRepBuilderAPI_MakeWire 
    brepBuilderAPI_MakeWire_AddWire mkWire aWire
    brepBuilderAPI_MakeWire_AddWire mkWire aMirroredWire
    aWireProfile <- brepBuilderAPI_MakeWire_Wire mkWire
    aFaceProfile <- brepBuilderAPI_MakeFace_Wire aWireProfile
    prismVec <- new_GP_Vec 0 0 theHeight
    prismBody <- brepPrimAPI_MakePrism aFaceProfile prismVec

    filletedBody <- filletShape (pure . pure . pure $ (theThickness/12)) prismBody

    neckLocation <- new_GP_Pnt 0 0 theHeight
    neckAxis <- gp_Dir_DZ
    neckAx2 <- new_GP_Ax2 neckLocation neckAxis
    let neckRadius = theThickness / 4
    let neckHeight = theHeight / 10
    mkCylinder <- new_BRepPrimAPI_MakeCylinder neckAx2 neckRadius neckHeight
    aNeck <- brepPrimAPI_MakeCylinder_Shape mkCylinder
    fusedNeckAndBody <- brepAlgoAPI_Fuse aNeck filletedBody

    bottleShell <- thickenSolid findNeck (-theThickness/50) (1e-3) fusedNeckAndBody

    aCyl1 <- new_Geom_CylindricalSurface neckAx2 (neckRadius * 0.99)
    aCyl2 <- new_Geom_CylindricalSurface neckAx2 (neckRadius * 1.05)

    aPnt <- new_GP_Pnt2d (2*pi) (neckHeight/2)
    aDir <- new_GP_Dir2d (2*pi) (neckHeight/4)
    anAx2d <- new_GP_Ax2d aPnt aDir
    let aMajor = 2*pi
    let aMinor = neckHeight / 10
    anEllipse1 <- new_Geom2d_Ellipse anAx2d aMajor aMinor
    anEllipse2 <- new_Geom2d_Ellipse anAx2d aMajor (aMinor/4)
    anArc1 <- new_Geom2d_TrimmedCurve_fromEllipse anEllipse1 0 pi
    anArc2 <- new_Geom2d_TrimmedCurve_fromEllipse anEllipse2 0 pi

    anEllipsePnt1 <- geom2d_Ellipse_Value anEllipse1 0
    anEllipsePnt2 <- geom2d_Ellipse_Value anEllipse1 pi
    
    aSegment <- gce2d_MakeSegment anEllipsePnt1 anEllipsePnt2

    anEdge1OnSurf1 <- brepBuilderAPI_MakeEdge_fromArcAndCylinder anArc1 aCyl1
    anEdge2OnSurf1 <- brepBuilderAPI_MakeEdge_fromArcAndCylinder aSegment aCyl1
    anEdge1OnSurf2 <- brepBuilderAPI_MakeEdge_fromArcAndCylinder anArc2 aCyl2
    anEdge2OnSurf2 <- brepBuilderAPI_MakeEdge_fromArcAndCylinder aSegment aCyl2

    threadingWire1 <- brepBuilderAPI_MakeWire_2Edges anEdge1OnSurf1 anEdge2OnSurf1
    threadingWire2 <- brepBuilderAPI_MakeWire_2Edges anEdge1OnSurf2 anEdge2OnSurf2
    brepLib_BuildCurves3d threadingWire1
    brepLib_BuildCurves3d threadingWire2

    thruSections <- new_BRepOffsetAPI_ThruSections 1
    brepOffsetAPI_ThruSections_AddWire thruSections threadingWire1
    brepOffsetAPI_ThruSections_AddWire thruSections threadingWire2
    brepOffsetAPI_ThruSections_CheckCompatibility thruSections 0

    thread <- brepOffsetAPI_ThruSections_Shape thruSections

    res <- new_TopoDS_Compound
    builder <- new_BRepBuilder 
    brep_Builder_MakeCompound builder res
    brep_Builder_Add builder res bottleShell
    brep_Builder_Add builder res thread

    return (castPtr res)

someFunc :: IO ()
someFunc = do
    shape <- makeBottleHS 50 60 50
    withCString "haskell.stl" (saveShapeSTL 0.01 shape) >>= print

--}
