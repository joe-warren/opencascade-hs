#ifndef BOTTLE_H
#define BOTTLE_H

#ifdef __cplusplus
extern "C" {
#else
typedef void TopoDS_Shape;
typedef void TopoDS_Edge;
typedef void TopoDS_Wire;
typedef void TopoDS_Face;
typedef void TopoDS_Compound;
typedef void gp_Pnt;
typedef void gp_Ax1;
typedef void gp_Ax2;
typedef void gp_Ax2d;
typedef void gp_Pnt2d;
typedef void gp_Dir;
typedef void gp_Dir2d;
typedef void gp_Vec;
typedef void gp_Trsf;
typedef void BRep_Builder;
typedef void BRepBuilderAPI_MakeWire;
typedef void BRepFilletAPI_MakeFillet;
typedef void TopAbs_ShapeEnum;
typedef void TopExp_Explorer;
typedef void BRepPrimAPI_MakeCylinder;
typedef void TopTools_ListOfShape;
typedef void BRepOffsetAPI_MakeThickSolid;
typedef void Geom2d_Ellipse;
typedef void BRepOffsetAPI_ThruSections;
#define Handle(X) void

#endif

gp_Pnt * hs_new_GP_Pnt(double x, double y, double z);

gp_Pnt2d * hs_new_GP_Pnt2d(double x, double y);

gp_Dir2d * hs_new_GP_Dir2d(double x, double y);

gp_Vec * hs_new_GP_Vec(double x, double y, double z) ;

Handle(Geom_TrimmedCurve) * hs_GCMakeArcOfCircle(gp_Pnt * p1, gp_Pnt * mid, gp_Pnt * p2);

Handle(Geom_TrimmedCurve) * hs_GC_MakeSegment(gp_Pnt * a, gp_Pnt * b);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdgeTrimmedCurve(Handle(Geom_TrimmedCurve) * curve) ;

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_2Edges(TopoDS_Edge * a, TopoDS_Edge * b);

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_3Edges(TopoDS_Edge * a, TopoDS_Edge * b, TopoDS_Edge * c);

gp_Ax1 * hs_gp_Ax1_OX(); 

gp_Ax1 * hs_gp_Ax1_OY(); 

gp_Ax1 * hs_gp_Ax1_OZ(); 

gp_Dir * hs_gp_Dir_DX(); 

gp_Dir * hs_gp_Dir_DY(); 

gp_Dir * hs_gp_Dir_DZ(); 

gp_Trsf * hs_new_Trsf();

void hs_Trsf_SetMirror(gp_Trsf * theTransform, gp_Ax1 *theAxis);

BRepBuilderAPI_MakeWire * hs_new_BRepBuilderAPI_MakeWire();

void hs_BRepBuilderAPI_MakeWire_AddWire(BRepBuilderAPI_MakeWire * theBuilder, TopoDS_Wire * theWire);

TopoDS_Wire * hs_BRepBuilderAPI_MakeWire_Wire(BRepBuilderAPI_MakeWire * theBuilder);

TopoDS_Face * hs_BRepBuilderAPI_MakeFace_Wire(TopoDS_Wire * theWire);

TopoDS_Shape * hs_BRepPrimAPI_MakePrism(TopoDS_Face * theFace, gp_Vec * theVector);

BRepFilletAPI_MakeFillet * hs_new_BRepFilletAPI_MakeFillet(TopoDS_Shape * theBody);

void hs_BRepFilletAPI_MakeFillet_Add(BRepFilletAPI_MakeFillet * theFillet, double r, TopoDS_Edge * theEdge);

TopoDS_Shape * hs_BRepFilletAPI_MakeFillet_Shape(BRepFilletAPI_MakeFillet * theFillet);

TopAbs_ShapeEnum * hs_new_TopAbs_EDGE();

TopAbs_ShapeEnum * hs_new_TopAbs_FACE();

TopExp_Explorer * hs_new_TopExp_Explorer(TopoDS_Shape * theShape, TopAbs_ShapeEnum * toFind);

bool hs_TopExp_Explorer_More(TopExp_Explorer * theExplorer);

TopoDS_Shape * hs_TopExp_Explorer_Current(TopExp_Explorer * theExplorer);

void hs_TopExp_Explorer_Next(TopExp_Explorer * theExplorer);

Handle(Geom_Surface) * hs_BRep_Tool_Surface(TopoDS_Face * aFace);

bool hs_Geom_Surface_isPlane(Handle(Geom_Surface) * aSurface);

gp_Pnt * hs_Geom_Plane_Location(Handle(Geom_Plane) * aPlane);

double hs_gp_Pnt_X(gp_Pnt * pnt);

double hs_gp_Pnt_Y(gp_Pnt * pnt);

double hs_gp_Pnt_Z(gp_Pnt * pnt);

gp_Ax2 * hs_new_gp_Ax2(gp_Pnt * pnt, gp_Dir * dir);

gp_Ax2d * hs_new_gp_Ax2d(gp_Pnt2d * pnt, gp_Dir2d * dir);

BRepPrimAPI_MakeCylinder * hs_new_BRepPrimAPI_MakeCylinder(gp_Ax2 * ax2, double radius, double height);

TopoDS_Shape * hs_BRepPrimAPI_MakeCylinder_Shape(BRepPrimAPI_MakeCylinder * theBuilder);

TopoDS_Shape * hs_BRepAlgoAPI_Fuse(TopoDS_Shape *a, TopoDS_Shape *b);

TopTools_ListOfShape * hs_new_TopTools_ListOfShape();

void hs_TopTools_ListOfShape_Append(TopTools_ListOfShape * theList, TopoDS_Shape* theShape);

BRepOffsetAPI_MakeThickSolid * hs_new_BRepOffsetAPI_MakeThickSolid();

void hs_BRepOffsetAPI_MakeThickSolid_MakeThickSolidByJoin(
            BRepOffsetAPI_MakeThickSolid *theMaker,
            TopoDS_Shape* theShape,
            TopTools_ListOfShape * closingFaces,
            double thickness,
            double tolerance);

TopoDS_Shape * hs_BRepOffsetAPI_MakeThickSolid_Shape(BRepOffsetAPI_MakeThickSolid * theMaker);

Handle(Geom_CylindricalSurface) * hs_new_GeomCylindricalSurface(gp_Ax2 * ax2, double r);

Geom2d_Ellipse * hs_new_Geom2d_Ellipse(gp_Ax2d *ax2, double rMajor, double rMinor);

Handle(Geom2d_TrimmedCurve) * hs_new_Geom2d_Trimmed_Curve_fromEllipse(Geom2d_Ellipse * theEllipse, double r1, double r2);

gp_Pnt2d * hs_Geom2d_Ellipse_Value(Geom2d_Ellipse * theEllipse, double theta);

Handle(Geom2d_TrimmedCurve) * hs_GCE2d_MakeSegment(gp_Pnt2d * pnt1, gp_Pnt2d * pnt2);

TopoDS_Edge * hs_BRepBuilderAPI_MakeEdge_fromArcAndCylinder(Handle(Geom2d_TrimmedCurve) * theArc, Handle(Geom_CylindricalSurface) * theSurface);

void hs_BRepLib_BuildCurves3d(TopoDS_Wire * wire); 

BRepOffsetAPI_ThruSections * hs_new_BRepOffsetAPI_ThruSections(bool isSolid);

void hs_BRepOffsetAPI_ThruSections_AddWire(BRepOffsetAPI_ThruSections * thruSections, TopoDS_Wire * theWire);

void hs_BRepOffsetAPI_ThruSections_CheckCompatibility(BRepOffsetAPI_ThruSections * thruSections, bool check);

TopoDS_Shape * hs_BRepOffsetAPI_ThruSections_Shape(BRepOffsetAPI_ThruSections * thruSections);

TopoDS_Compound * hs_new_TopoDS_Compound();

BRep_Builder * hs_new_BRepBuilder();

void hs_BRep_Builder_MakeCompound(BRep_Builder * theBuilder, TopoDS_Compound * theCompound);

void hs_BRep_Builder_Add(BRep_Builder * theBuilder, TopoDS_Compound * theCompound, TopoDS_Shape * theShape);

TopoDS_Shape * MakeBottle (const double theWidth,
                           const double theHeight,
                           const double theThickness);

int SaveShapeSTL(double res, TopoDS_Shape *shape, char* filename);

#ifdef __cplusplus
}
#endif
#endif /* BOTTLE_H */
