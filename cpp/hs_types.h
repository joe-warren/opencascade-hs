#ifndef HS_TYPES_H
#define HS_TYPES_H

#ifndef __cplusplus

typedef void TopoDS_Shape;
typedef void TopoDS_Edge;
typedef void TopoDS_Wire;
typedef void TopoDS_Face;
typedef void TopoDS_Compound;
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
typedef void gp_Trsf2d;
typedef void BRep_Builder;
typedef void BRepBuilderAPI_Transform;
typedef void BRepBuilderAPI_MakeWire;
typedef void BRepFilletAPI_MakeFillet;
typedef void TopExp_Explorer;
typedef void BRepPrimAPI_MakeCylinder;
typedef void TopTools_ListOfShape;
typedef void BRepOffsetAPI_MakeThickSolid;
typedef void Geom2d_Ellipse;
typedef void BRepOffsetAPI_ThruSections;
#define Handle(X) void

#endif // __cplusplus
#endif // HS_TYPES_H


