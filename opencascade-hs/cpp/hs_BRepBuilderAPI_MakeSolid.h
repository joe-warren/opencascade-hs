#ifndef HS_BREPBUILDERAPI_MAKESOLID_H
#define HS_BREPBUILDERAPI_MAKESOLID_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepBuilderAPI_MakeSolid * hs_new_BRepBuilderAPI_MakeSolid();

void hs_delete_BRepBuilderAPI_MakeSolid(BRepBuilderAPI_MakeSolid * builder);

void hs_BRepBuilderAPI_MakeSolid_add(BRepBuilderAPI_MakeSolid * builder, TopoDS_Shell * shell);

TopoDS_Solid * hs_BRepBuilderAPI_MakeSolid_solid(BRepBuilderAPI_MakeSolid * builder);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPBUILDERAPI_MAKESOLID_H
