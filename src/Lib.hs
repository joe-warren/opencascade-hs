{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}
module Lib
    ( someFunc
    ) where
import Foreign.C
import Foreign.Ptr

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

foreign import capi unsafe "bottle.h hs_Trsf_SetMirror" gp_Trsf_Set_Mirror :: Ptr GP_Trsf -> Ptr GP_Ax1 -> IO ()

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

foreign import capi unsafe "bottle.h hs_new_gp_Ax2" new_gp_Ax2 :: Ptr GP_Pnt -> Ptr GP_Dir -> IO (Ptr GP_Ax2)

foreign import capi unsafe "bottle.h hs_new_gp_Ax2d" new_gp_Ax2d :: Ptr GP_Pnt2d -> Ptr GP_Dir2d -> IO (Ptr GP_Ax2d)

foreign import capi unsafe "bottle.h hs_new_BRepPrimAPI_MakeCylinder" new_BRepPrimAPI_MakeCylinder :: Ptr GP_Ax2 -> CDouble -> CDouble -> IO (Ptr BRepPrimAPI_MakeCylinder)

foreign import capi unsafe "bottle.h hs_BRepPrimAPI_MakeCylinder_Shape" brepPrimAPI_MakeCylinder_Shape :: Ptr BRepPrimAPI_MakeCylinder -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_BRepAlgoAPI_Fuse" brepAlgoAPI_Fuse :: Ptr TopoDS_Shape -> Ptr TopoDS_Shape -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_TopTools_ListOfShape" new_TopTools_ListOfShape :: IO (Ptr TopTools_ListOfShape)

foreign import capi unsafe "bottle.h hs_TopTools_ListOfShape_Append" topTools_ListOfShape_Append :: Ptr TopTools_ListOfShape -> Ptr TopoDS_Shape -> IO ()

foreign import capi unsafe "bottle.h hs_new_BRepOffsetAPI_MakeThickSolid" new_BRepOffsetAPI_MakeThickSolid :: IO (Ptr BRepOffsetAPI_MakeThickSolid)

foreign import capi unsafe "bottle.h hs_BRepOffsetAPI_MakeThickSolid_MakeThickSolidByJoin" brepOffsetAPI_MakeThickSolid_MakeThickSolidByJoin :: Ptr BRepOffsetAPI_MakeThickSolid -> Ptr TopoDS_Shape -> Ptr TopTools_ListOfShape -> CDouble -> CDouble -> IO ()

foreign import capi unsafe "bottle.h hs_BRepOffsetAPI_MakeThickSolid_Shape" brepOffsetAPI_MakeThickSolid_Shape :: Ptr BRepOffsetAPI_MakeThickSolid -> IO (Ptr TopoDS_Shape)

foreign import capi unsafe "bottle.h hs_new_GeomCylindricalSurface" new_GeomCylindricalSurface :: Ptr GP_Ax2 -> CDouble -> IO (Ptr Handle_Geom_CylindricalSurface)

foreign import capi unsafe "bottle.h hs_new_Geom2d_Ellipse" new_Geom2s_Ellipse :: Ptr GP_Ax2d -> CDouble -> CDouble -> IO (Ptr Geom2d_Ellipse)

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
foreign import capi unsafe "bottle.h MakeBottle" makeBottle :: CDouble -> CDouble -> CDouble -> IO (Ptr TopoDS_Shape)

someFunc :: IO ()
someFunc = do
    shape <- makeBottle 50 100 50
    withCString "haskell.stl" (saveShapeSTL 0.01 shape) >>= print
