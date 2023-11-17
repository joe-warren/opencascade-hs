#ifndef HS_TYPES_H
#define HS_TYPES_H

#ifndef __cplusplus

typedef void TopoDS_Shape;
typedef void TopoDS_Vertex;
typedef void TopoDS_Edge;
typedef void TopoDS_Face;
typedef void TopoDS_Wire;
typedef void TopoDS_Compound;
typedef void TopoDS_CompSolid;
typedef void TopoDS_Solid;
typedef void TopoDS_Shell;
typedef void TopLoc_Location;
typedef int TopAbs_Orientation;
typedef int TopAbs_ShapeEnum;
typedef void gp_Pnt;
typedef void gp_Ax1;
typedef void gp_Ax2;
typedef void gp_Ax2d;
typedef void gp_Pnt2d;
typedef void gp_Dir;
typedef void gp_Dir2d;
typedef void gp_Vec;
typedef void gp_Vec2d;
typedef void gp_Trsf;
typedef void gp_GTrsf;
typedef void gp_Trsf2d;
typedef void BRep_Builder;
typedef void BRepBuilderAPI_Transform;
typedef void BRepBuilderAPI_MakeWire;
typedef void BRepBuilderAPI_MakeFace;
typedef int BRepBuilderAPI_WireError;
typedef int BRepBuilderAPI_FaceError;
typedef void BRepFilletAPI_MakeFillet;
typedef void BRepPrimAPI_MakeBox;
typedef void TopExp_Explorer;
typedef void BRepPrimAPI_MakeCylinder;
typedef void TopTools_ListOfShape;
typedef void BRepOffsetAPI_MakeThickSolid;
typedef void Geom2d_Ellipse;
typedef void BRepOffsetAPI_ThruSections;
typedef void BRepMesh_IncrementalMesh;
typedef void StlAPI_Writer;
typedef void STEPControl_Writer;
typedef int STEPControl_StepModelType;
typedef int IFSelect_ReturnStatus;
#define Handle(X) void

#endif // __cplusplus
#endif // HS_TYPES_H


