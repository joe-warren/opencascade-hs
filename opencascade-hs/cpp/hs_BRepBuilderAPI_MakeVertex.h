#ifndef HS_BREPBUILDERAPI_MAKEVERTEX_H
#define HS_BREPBUILDERAPI_MAKEVERTEX_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepBuilderAPI_MakeVertex * hs_new_BRepBuilderAPI_MakeVertex_fromPnt(gp_Pnt* pnt);

void hs_delete_BRepBuilderAPI_MakeVertex(BRepBuilderAPI_MakeVertex* builder);

TopoDS_Vertex * hs_BRepBuilderAPI_MakeVertex_vertex(BRepBuilderAPI_MakeVertex * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_MAKEVERTEX_H
