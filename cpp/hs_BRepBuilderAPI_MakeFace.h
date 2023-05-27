#ifndef HS_BREPBUILDERAPI_MAKEFACE_H
#define HS_BREPBUILDERAPI_MAKEFACE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace();

void hs_delete_BRepBuilderAPI_MakeFace(BRepBuilderAPI_MakeFace * builder);

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromFace(TopoDS_Face * face);

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromSurface(Handle(Geom_Surface)* surface, double tolerance);

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromSurfaceAndBounds(Handle(Geom_Surface)* surface, double uMin, double uMax, double vMin, double vMax, double tolerance);

BRepBuilderAPI_MakeFace * hs_new_BRepBuilderAPI_MakeFace_fromSurfaceAndWire(Handle(Geom_Surface)* surface, TopoDS_Wire* wire, bool inside);

void hs_BRepBuilderAPI_MakeFace_Add(BRepBuilderAPI_MakeFace* builder, TopoDS_Wire* wire);

bool hs_BRepBuilderAPI_MakeFace_IsDone(BRepBuilderAPI_MakeFace* builder);

BRepBuilderAPI_FaceError hs_BRepBuilderAPI_MakeFace_Error(BRepBuilderAPI_MakeFace * builder);

TopoDS_Face * hs_BRepBuilderAPI_MakeFace_Face(BRepBuilderAPI_MakeFace * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_MAKEFACE_H
