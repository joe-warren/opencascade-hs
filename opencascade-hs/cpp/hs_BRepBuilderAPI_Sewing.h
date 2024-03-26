#ifndef HS_BREPBUILDERAPI_SEWING_H
#define HS_BREPBUILDERAPI_SEWING_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepBuilderAPI_Sewing * hs_new_BRepBuilderAPI_Sewing(double tolerance , bool option1, bool option2, bool option3, bool option4);

void hs_delete_BRepBuilderAPI_Sewing(BRepBuilderAPI_Sewing * builder);

void hs_BRepBuilderAPI_Sewing_load(BRepBuilderAPI_Sewing * builder, TopoDS_Shape * shape);

void hs_BRepBuilderAPI_Sewing_add(BRepBuilderAPI_Sewing * builder, TopoDS_Shape * shape);

void hs_BRepBuilderAPI_Sewing_perform(BRepBuilderAPI_Sewing * builder);

TopoDS_Shape * hs_BRepBuilderAPI_Sewing_sewedShape(BRepBuilderAPI_Sewing * builder);

int hs_BRepBuilderAPI_Sewing_nbFreeEdges(BRepBuilderAPI_Sewing * builder);

int hs_BRepBuilderAPI_Sewing_nbContigousEdges(BRepBuilderAPI_Sewing * builder);

int hs_BRepBuilderAPI_Sewing_nbMultipleEdges(BRepBuilderAPI_Sewing * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_SEWING_H
